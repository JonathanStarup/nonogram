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
    let paint = Ref.fresh(rc, Color.Black);
    runApp({
        title = "Demo",
        width = 800,
        height = 450,
        init = g -> {
            Graphics.fillRect({x = 0, y = 0}, {width = 800, height = 450}, Graphics.Color.Black, g)
        },
        mouseListener = {
            onPress = event -> Some(d -> {
                if (event#but == MouseButton.Left) Ref.put(Color.Red, paint)
                else Ref.put(Color.Black, paint);
                Graphics.fillCircle({x = event#x, y = event#y}, {radius = 20}, Ref.get(paint), d)
            }),
            onRelease = _ -> None,
            onDrag = event -> Some(d -> {
                Graphics.fillCircle({x = event#x, y = event#y}, {radius = 20}, Ref.get(paint), d)
            })
        }
    })
}
