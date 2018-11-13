function htmlToCanvas(node, canvas, width, height, whenDone) {
    const context = canvas.getContext("2d");
    const data =
        '<svg xmlns="http://www.w3.org/2000/svg" width="'+width+'px" height="'+height+'px">' +
              '<foreignObject width="100%" height="100%">' +
                  '<div xmlns="http://www.w3.org/1999/xhtml">' +
                      node.innerHTML +
                  '</div>' +
              '</foreignObject>' +
          '</svg>';
    const image = new Image();
    // image.crossOrigin = 'Anonymous';
    image.addEventListener('error', (err) => {
        throw err;
    });
    image.setAttribute('crossOrigin', 'anonymous');
    //const svg = new Blob([data], {type: 'image/svg+xml;charset=utf-8'});
    const url = 'data:image/svg+xml;base64,' + btoa(data);
    //const url = URL.createObjectURL(svg);
    image.addEventListener('load', () => {
        URL.revokeObjectURL(url);
        context.drawImage(image, 0, 0);
        if (whenDone) whenDone(canvas);
    }, false);
    image.src = url;
    // document.body.appendChild(image);
    // context.canvas.width = width;
    // context.canvas.height = height;
}

function imageToCanvas(src, transform, canvas, x, y, width, height, whenDone) {
    const context = canvas.getContext('2d');
    const image = new Image();
    //image.style.cssText = document.defaultView.getComputedStyle(cloneFrom, '').cssText;
    image.setAttribute('crossOrigin', 'anonymous');
    image.addEventListener('error', (err) => {
        throw err;
    });
    image.addEventListener('load', () => {
        transform(image, context);
        context.drawImage(image, 0, 0, width, height);
        if (whenDone) whenDone(canvas);
    }, false);
    image.src = src;
    //document.body.appendChild(image);
}

module.exports = {
    html: htmlToCanvas,
    image: imageToCanvas
};
