mod Examples.Paint {
    use Graphics.drawRect

use Graphics.Color
use Graphics.MouseButton
use Graphics.runApp
use Graphics.MouseListener

pub def main(): Unit \ IO = region rc {
    let width = 1200;
    let height = 800;
    def sizeFont(c) = {
        size = 20f32,
        color = c,
        bold = false,
        italic = false
    };
    let isPainting = Ref.fresh(rc, false);
    let paint = Ref.fresh(rc, Color.Orange);
    let size = Ref.fresh(rc, 5);
    def drawColorIndicator(d) = {
        Graphics.drawOval({x = width/2, y = 0}, {width = 140, height = 70}, Color.White, d);
        Graphics.drawOval({x = width/2, y = 0}, {width = 135, height = 65}, Color.Black, d);
        Graphics.drawOval({x = width/2, y = 0}, {width = 130, height = 60}, Ref.get(paint), d)
    };
    def drawSizeIndicator(d) = {
        Graphics.drawOval({x = width/2, y = height}, {width = 140, height = 70}, Color.Black, d);
        Graphics.drawOval({x = width/2, y = height}, {width = 135, height = 65}, Color.White, d);
        Graphics.drawText({x = width/2, y = height-8}, Ref.get(size) |> ToString.toString, sizeFont(Color.Black), d)
    };
    def drawUI(d) = {
        drawColorIndicator(d);
        drawSizeIndicator(d)
    };
    def paintSpot(center, d) = {
        Graphics.drawCircle(center, {radius = Ref.get(size) * 5}, Ref.get(paint), d)
    };
    def init(d) = {
        drawRect({x = 0, y = 0}, {width = width, height = height}, Color.White, d);
        drawUI(d)
    };
    def onPress(event) = {
        isPainting |> Ref.put(match event#but {
            case MouseButton.Left => true
            case _ => false
        });
        match event#but {
            case MouseButton.Left =>
                Some(d -> {
                    paintSpot({x = event#x, y = event#y}, d);
                    drawUI(d)
                })
            case MouseButton.Right =>
                Ref.transform(nextColor, paint);
                Some(drawUI)
            case _ => None
        }
    };
    runApp({
        title = "Demo",
        width = width,
        height = height,
        init = init,
        mouseListener = MouseListener.mk({
            onPress = onPress,
            onRelease = _ -> None,
            onDrag = event -> {
                if (Ref.get(isPainting)) Some(d -> {
                    paintSpot({x = event#x, y = event#y}, d);
                    drawUI(d)
                }) else None
            },
            onScroll = event -> {
                if (event#amount > 0 and Ref.get(size) > 1) Some(d -> {
                    Ref.transform(val -> val - 1, size);
                    drawUI(d)
                }) else if (event#amount < 0) Some(d -> {
                    Ref.transform(val -> val + 1, size);
                    drawUI(d)
                }) else None
            }
        })
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
