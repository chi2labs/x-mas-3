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


