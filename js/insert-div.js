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


function createArrowOverlay(top, left, width, height, duration = 3000, arrowSymbol = "⇐⇒", fontSize =60, bgColor = "rgba(0, 0, 0, 0.3)") {
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
    overlayDiv.style.color = "red"; /* Solid red arrow color */
    overlayDiv.innerHTML = arrowSymbol;
    document.body.appendChild(overlayDiv);

    /* Set a timeout to remove the overlay after the specified duration */
    setTimeout(() => {
        document.body.removeChild(overlayDiv);
    }, duration);
}

/* Example usage: create an arrow overlay at coordinates (100, 100) with a size of 200x100 pixels,
   displaying a custom arrow symbol "⇐⇒" with a font size of 40, bold text, solid red arrow color,
   on a semi-transparent black background, that disappears after 5 seconds */



