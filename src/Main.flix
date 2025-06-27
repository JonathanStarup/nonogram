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

def main(): Unit \ IO = region rc {
    let isPainting = Ref.fresh(rc, false);
    let paint = Ref.fresh(rc, Color.Black);
    runApp({
        title = "Demo",
        width = 800,
        height = 450,
        init = g -> {
            Graphics.fillRect({x = 0, y = 0}, {width = 800, height = 450}, Graphics.Color.Black, g)
        },
        mouseListener = {
            onPress = event -> match event#but {
                case MouseButton.Left => Some(d -> {
                    Ref.put(true, isPainting);
                    Graphics.fillCircle({x = event#x, y = event#y}, {radius = 20}, Ref.get(paint), d)
                })
                case MouseButton.Right =>
                    Ref.transform(nextColor, paint);
                    None
                case _ => None
            },
            onRelease = _ -> None,
            onDrag = event -> Some(d -> {
                Graphics.fillCircle({x = event#x, y = event#y}, {radius = 20}, Ref.get(paint), d)
            })
        }
    })
}

def nextColor(c: Color): Color = match c {
    case Color.Black => Color.Red
    case Color.Red   => Color.Black
}
