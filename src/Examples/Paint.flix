mod Examples.Paint {

use Graphics.Color
use Graphics.MouseButton
use Graphics.runApp

pub def main(): Unit \ IO = region rc {
    let isPainting = Ref.fresh(rc, false);
    let paint = Ref.fresh(rc, Color.Orange);
    let width = 1200;
    let height = 800;
    def paintCornerBlip(d) = {
        Graphics.fillRect({x = 0, y = 0}, {width = 14, height = 14}, Color.White, d);
        Graphics.fillRect({x = 0, y = 0}, {width = 12, height = 12}, Color.Black, d);
        Graphics.fillRect({x = 0, y = 0}, {width = 10, height = 10}, Ref.get(paint), d)
    };
    def init(d) = {
        paintCornerBlip(d)
    };
    def onPress(event) = {
        isPainting |> Ref.put(match event#but {
            case MouseButton.Left => true
            case _ => false
        });
        match event#but {
            case MouseButton.Left =>
                Some(d -> {
                    Graphics.fillCircle({x = event#x, y = event#y}, {radius = 20}, Ref.get(paint), d);
                    paintCornerBlip(d)
                })
            case MouseButton.Right =>
                Ref.transform(nextColor, paint);
                Some(paintCornerBlip)
            case _ => None
        }
    };
    runApp({
        title = "Demo",
        width = width,
        height = height,
        init = init,
        mouseListener = {
            onPress = onPress,
            onRelease = _ -> None,
            onDrag = event -> {
                if (Ref.get(isPainting)) Some(d -> {
                    Graphics.fillCircle({x = event#x, y = event#y}, {radius = 20}, Ref.get(paint), d);
                    paintCornerBlip(d)
                }) else None
            }
        }
    })
}

def nextColor(c: Color): Color = match c {
    case Color.Black   => Color.Yellow
    case Color.Blue    => Color.Black
    case Color.Cyan    => Color.Blue
    case Color.Gray    => Color.Cyan
    case Color.Green   => Color.Gray
    case Color.Magenta => Color.Green
    case Color.Orange  => Color.Magenta
    case Color.Pink    => Color.Orange
    case Color.Red     => Color.Pink
    case Color.White   => Color.Red
    case Color.Yellow  => Color.White
}

}
