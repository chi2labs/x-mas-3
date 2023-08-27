function createOverlay(top, left, width, height, duration = 3000, text = "⇐⇒", fontSize = 20, bgColor = "rgba(255, 0, 0, 0.3)") {
    const overlayDiv = document.createElement("div");
    overlayDiv.style.position = "absolute";
    overlayDiv.style.backgroundColor = bgColor;
    overlayDiv.style.border = "2px solid red";
    overlayDiv.style.width = width + "px";
    overlayDiv.style.height = height + "px";
    overlayDiv.style.color = "white";
    overlayDiv.style.textAlign = "center";
    overlayDiv.style.lineHeight = height + "px";
    overlayDiv.style.fontSize = fontSize + "px";
    overlayDiv.style.top = top + "px";
    overlayDiv.style.left = left + "px";
    overlayDiv.style.zIndex = "9999";
    overlayDiv.innerHTML = text;
    document.body.appendChild(overlayDiv);

    setTimeout(() => {
        document.body.removeChild(overlayDiv);
    }, duration);
}


function createArrowOverlay(top, left, width, height, duration = 3000, arrowSymbol = "⇐⇒", fontSize =60, bgColor = "rgba(0, 0, 0, 0)") {
    /* Create a div to hold the arrow symbol */
    const overlayDiv = document.createElement("div");
    overlayDiv.style.position = "absolute";
    overlayDiv.style.width = width + "px";
    overlayDiv.style.height = height + "px";
    overlayDiv.style.top = top + "px";
    overlayDiv.style.left = left + "px";
    overlayDiv.style.zIndex = "9999";
    overlayDiv.style.backgroundColor = bgColor;
    overlayDiv.style.textAlign = "center";
    overlayDiv.style.lineHeight = height + "px";
    overlayDiv.style.fontSize = fontSize + "px";
    overlayDiv.style.fontWeight = "bold"; /* Make the text bold */
    overlayDiv.style.color = "white"; /* Solid red arrow color */
    overlayDiv.innerHTML = arrowSymbol;
    document.body.appendChild(overlayDiv);

    /* Set a timeout to remove the overlay after the specified duration */
    setTimeout(() => {
        document.body.removeChild(overlayDiv);
    }, duration);
}


function insertTeletypeDiv(text1, text2, x, y, duration = 10000) {
    /* Set the z-index for the teletype div */
    const zIndex = 9999;

    /* Create a div element */
    const div = document.createElement("div");
    div.style.position = "absolute";
    div.style.left = x + "px";
    div.style.top = y + "px";
    div.style.zIndex = zIndex.toString(); /* Set the z-index */
    div.style.backgroundColor = "rgba(0, 0, 0, 0.9)"; /* Semi-transparent black background */
    div.style.width = "200px"; /* Fix the width at 200 pixels */
    div.style.height = "800px"; /* Fix the height at 800 pixels */
    div.style.whiteSpace = "pre-line"; /* Allow line breaks */
    div.style.color = "white"; /* Set the text color to white */
    div.style.fontFamily = "Courier New, monospace"; /* Set the font to fixed-width */
    div.style.padding = "10px";
    document.body.appendChild(div);

    /* Set the initial text */
    div.textContent = text1;

    /* Define a function to add the second text in a teletype fashion */
    function typeText(text, index) {
        if (index < text.length) {
            div.textContent += text.charAt(index);
            setTimeout(function () {
                typeText(text, index + 1);
            }, 30); /* Adjust typing speed here (e.g., 100 milliseconds per character) */
        }
    }

    /* Start typing the second text after a delay (e.g., 1 second) */
    setTimeout(function () {
        typeText(text2, 0);
    }, 1000); /* Adjust the delay as needed (e.g., 1000 milliseconds for 1 second) */
    
    setTimeout(() => {
        document.body.removeChild(div);
    }, duration);
    
}

/* Example usage: Insert a teletype div at coordinates (100, 100) with "Hello, " displayed immediately,
   and "World!" appearing in teletype after 1 second, with a semi-transparent black background, white text,
   a fixed width of 200 pixels, fixed height of 800 pixels, line breaks, and a z-index of 9999 */
/*
insertTeletypeDiv("\n\n '/\\_/\\ \n( o.o )\n'> ^ <\n\nGatai the AI\n", "World!S\nThis is a new line.", 0, 0);
*/