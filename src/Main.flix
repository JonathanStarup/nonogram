import java.awt.{Graphics => JGraphics}
import java.awt.Color
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

def main(): Unit \ IO + Chan = {
    def paintCursor(x, y, g) = {
        Graphics.fillCircle({x = x, y = y}, {radius = 20}, Graphics.Color.Red, g)
    };
    let {frame = _frame, waitForExit} = makeFrame({
        title = "Demo",
        width = 800,
        height = 450,
        init = g -> {
            Graphics.fillRect({x = 0, y = 0}, {width = 800, height = 450}, Graphics.Color.Black, g)
        },
        mouseListener = {
            onPress = (event, g) -> {
                paintCursor(event.getX(), event.getY(), g);
                PaintEvent.Repaint
            },
            onRelease = (_, _) -> PaintEvent.Nothing,
            onDrag = (event, g) -> {
                paintCursor(event.getX(), event.getY(), g);
                PaintEvent.Repaint
            }
        }
    });
    waitForExit()
}

type alias FrameConfig[ef1: Eff, ef2: Eff, ef3: Eff, ef4: Eff] = {
    title = String,
    height = Int32,
    width = Int32,
    init = Graphics2D -> Unit \ ef1,
    mouseListener = MouseListener[ef2, ef3, ef4]
}

type alias Frame = {
    frame = JFrame,
    waitForExit = Unit -> Unit \ Chan
}

def makeFrame(config: FrameConfig[ef1, ef2, ef3, ef4]): Frame \ IO + Chan + ef1 = {
    let {title, height, width, init, mouseListener} = config;
    let (send, recv) = Channel.buffered(1);
    def waitForClose() = {
        // Unit only has one value, so no non-determinism.
        unsafely NonDet run Channel.recv(recv);
        Channel.send((), send)
    };
    let f = new JFrame(title);
    f.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    f |> addOnExitListener(() -> Channel.send((), send));
    f.setSize(width, height);
    let image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    let initG = image.createGraphics();
    init(initG);
    initG.dispose();
    let base = new JComponent {
        def paint(_this: JComponent, g: JGraphics): Unit \ IO = {
            discard g.drawImage(image, 0, 0, unchecked_cast(null as ImageObserver))
        }
    };
    base |> addMouseListener(image, mouseListener);
    f.add(base);
    touchEff((ProxyEff.Proxy: ProxyEff[ef1]));
    f.setResizable(false);
    f.setVisible(true);
    {frame = f, waitForExit = waitForClose}
}

def addOnExitListener(f: Unit -> Unit \ ef, frame: JFrame): Unit \ IO = {
    frame.addWindowListener(new WindowListener {
        def windowDeiconified(_this: WindowListener, _: WindowEvent): Unit = ()
        def windowActivated(_this: WindowListener, _: WindowEvent): Unit = ()
        def windowIconified(_this: WindowListener, _: WindowEvent): Unit = ()
        def windowClosing(_this: WindowListener, _: WindowEvent): Unit = ()
        def windowClosed(_this: WindowListener, _: WindowEvent): Unit \ ef = {
            f()
        }
        def windowDeactivated(_this: WindowListener, _: WindowEvent): Unit = ()
        def windowOpened(_this: WindowListener, _: WindowEvent): Unit = ()
    })
}

enum PaintEvent {
    case Repaint, Nothing
}

enum ProxyEff[_: Eff] {
    case Proxy
}

def touchEff(_: ProxyEff[ef]): Unit \ ef = checked_ecast(())

type alias MouseListener[ef1: Eff, ef2: Eff, ef3: Eff] = {
    onPress = MouseEvent -> Graphics2D -> PaintEvent \ ef1,
    onRelease = MouseEvent -> Graphics2D -> PaintEvent \ ef2,
    onDrag = MouseEvent -> Graphics2D -> PaintEvent \ ef3
}

def addMouseListener(image: BufferedImage, listener: MouseListener[ef1, ef2, ef3], base: JComponent): Unit \ IO = {
    base.addMouseListener(new JMouseListener {
        def mouseExited(_this: JMouseListener, _event: MouseEvent): Unit = ()
        def mouseEntered(_this: JMouseListener, _event: MouseEvent): Unit = ()
        def mousePressed(_this: JMouseListener, event: MouseEvent): Unit \ ef1 + IO = {
            let g = image.createGraphics();
            let pEvent = listener#onPress(event, g);
            g.dispose();
            match pEvent {
                case PaintEvent.Nothing => ()
                case PaintEvent.Repaint => base.repaint()
            }
        }
        def mouseClicked(_this: JMouseListener, _event: MouseEvent): Unit = ()
        def mouseReleased(_this: JMouseListener, event: MouseEvent): Unit \ ef2 + IO = {
            let g = image.createGraphics();
            let pEvent = listener#onRelease(event, g);
            g.dispose();
            match pEvent {
                case PaintEvent.Nothing => ()
                case PaintEvent.Repaint => base.repaint()
            }
        }
    });
    base.addMouseMotionListener(new JMouseMotionListener {
        def mouseMoved(_this: JMouseMotionListener, _event: MouseEvent): Unit = ()
        def mouseDragged(_this: JMouseMotionListener, event: MouseEvent): Unit \ ef3 + IO = {
            let g = image.createGraphics();
            let pEvent = listener#onDrag(event, g);
            g.dispose();
            match pEvent {
                case PaintEvent.Nothing => ()
                case PaintEvent.Repaint => base.repaint()
            }
        }
    })
}

mod Graphics {
    import java.awt.{Color => JColor}
    import java.awt.Graphics2D

    pub enum Color {
        case Black
        case Red
    }

    pub def fillRect(corner: {x = Int32, y = Int32}, dim: {width = Int32, height = Int32}, color: Color, g: Graphics2D): Unit \ IO = {
        colorSwap(color, g);
        g.fillRect(corner#x, corner#y, dim#width, dim#height)
    }

    pub def fillCircle(center: {x = Int32, y = Int32}, radius: {radius = Int32}, color: Color, g: Graphics2D): Unit \ IO = {
        colorSwap(color, g);
        let offset = radius#radius / 2;
        g.fillOval(center#x - offset, center#y - offset, radius#radius, radius#radius)
    }

    def colorSwap(color: Color, g: Graphics2D): Unit \ IO =
        match color {
            case Color.Black => g.setColor(JColor.black)
            case Color.Red => g.setColor(JColor.red)
        }
}
