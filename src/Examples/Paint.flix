mod Examples.Paint {

import java.awt.{Graphics => JGraphics}
import java.awt.event.{MouseListener => JMouseListener}
import java.awt.event.{MouseMotionListener => JMouseMotionListener}
import java.awt.event.MouseEvent
import java.awt.event.WindowEvent
import java.awt.event.WindowListener
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.awt.image.ImageObserver
import javax.swing.JComponent
import javax.swing.JFrame

use Graphics.Color
use Graphics.MouseButton
use Graphics.runApp

pub def main(): Unit \ IO = region rc {
    let isPainting = Ref.fresh(rc, false);
    let paint = Ref.fresh(rc, Color.Orange);
    def paintCornerBlip(d) = {
        Graphics.fillRect({x = 0, y = 0}, {width = 14, height = 14}, Color.White, d);
        Graphics.fillRect({x = 0, y = 0}, {width = 12, height = 12}, Color.Black, d);
        Graphics.fillRect({x = 0, y = 0}, {width = 10, height = 10}, Ref.get(paint), d)
    };
    def init(d) = {
        Graphics.fillRect({x = 0, y = 0}, {width = 800, height = 450}, Color.Black, d);
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
        width = 800,
        height = 450,
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
