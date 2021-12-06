let newRawData = [];

let newRes = 32;
let factor = 512 / newRes;
{
  const img = document.querySelector("img");
  img.style = "";
  const canvas = document.createElement("canvas");
  canvas.style.width = "512px";
  canvas.style.height = "512px";
  canvas.width = img.width;
  canvas.height = img.height;

  const ctx = canvas.getContext("2d");
  ctx.drawImage(img, 0, 0);
  const imgData = ctx.getImageData(0, 0, img.width, img.height).data;

  let res = [];

  let getEvery = factor;

  for (let i = 0; i < img.width; i += factor) {
    for (let j = 0; j < img.height; j += factor) {
      {
        let x = imgData[4 * (i + j * img.width) + 0];
        let y = imgData[4 * (i + j * img.width) + 1];
        let z = imgData[4 * (i + j * img.width) + 2];
        let a = imgData[4 * (i + j * img.width) + 3];

        res.push({ x, y, z });
        newRawData.push(x);
        newRawData.push(y);
        newRawData.push(z);
        newRawData.push(a);
      }
    }
  }
  console.log(res);
}

{
  console.log(newRawData);
  let newCanvas = document.createElement("canvas");
  // newCanvas.style.width = "32px";
  // newCanvas.style.height = "32px";
  newCanvas.width = newRes;
  newCanvas.height = newRes;
  let newCtx = newCanvas.getContext("2d");
  let imageData = newCtx.getImageData(0, 0, newRes, newRes);

  for (let i = 0; i < newRes * newRes; i++) {
    for (let c = 0; c < 4; c++) {
      imageData.data[4 * i + c] = newRawData[4 * i + c];
      // imageData.data[4 * i + c] = 200;
    }
  }

  newCtx.putImageData(imageData, 0, 0);

  document.body.appendChild(newCanvas);
  newCanvas.toDataURL();
}
