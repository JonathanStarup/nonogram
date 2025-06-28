mod Graphics {
    import java.awt.{Color => JColor}
    import java.awt.{Graphics => JGraphics}
    import java.awt.event.{MouseListener => JMouseListener}
    import java.awt.event.{MouseMotionListener => JMouseMotionListener}
    import java.awt.event.MouseEvent
    import java.awt.event.WindowEvent
    import java.awt.event.WindowListener
    import java.awt.{Font => JFont}
    import java.awt.Graphics2D
    import java.awt.image.BufferedImage
    import java.awt.image.ImageObserver
    import javax.swing.JComponent
    import javax.swing.JFrame

    pub type alias App[ef1: Eff, ef2: Eff, ef3: Eff, ef4: Eff, ef5: Eff, ef6: Eff, ef7: Eff] = {
        title = String,
        height = Int32,
        width = Int32,
        init = Drawing -> Unit \ ef1,
        mouseListener = MouseListener[ef2, ef3, ef4, ef5, ef6, ef7]
    }

    pub enum Drawing {
        case Drawing(Graphics2D)
    }

    pub enum MouseButton with Eq {
        case Left
        case Middle
        case Right
        case Unknown
    }

    pub type alias MouseListener[ef1: Eff, ef2: Eff, ef3: Eff, ef4: Eff, ef5: Eff, ef6: Eff] = {
        onPress = {x = Int32, y = Int32, but = MouseButton} -> Option[Drawing -> Unit \ ef1] \ ef2,
        onRelease = {x = Int32, y = Int32, but = MouseButton} -> Option[Drawing -> Unit \ ef3] \ ef4,
        onDrag = {x = Int32, y = Int32} -> Option[Drawing -> Unit \ ef5] \ ef6
    }

    mod MouseListener {
        pub def empty(): MouseListener[{}, {}, {}, {}, {}, {}] = {
            onPress = _ -> None,
            onRelease = _ -> None,
            onDrag = _ -> None
        }
    }

    pub def runApp(config: App[ef1, ef2, ef3, ef4, ef5, ef6, ef7]): Unit \ IO + ef1 + ef2 + ef3 + ef4 + ef5 + ef6 + ef7 = {
        // The channel is enclosed so the Chan effect can be removed.
        checked_ecast(unsafely Chan run {
            let {frame = _, waitForExit} = makeFrame(config);
            // ef2+ are captured in listeners, assume that they happen.
            checked_ecast(());
            waitForExit()
        })
    }

    pub enum Color with Eq {
        case Black
        case Blue
        case Cyan
        case Gray
        case Green
        case Magenta
        case Orange
        case Pink
        case Red
        case White
        case Yellow
    }

    pub def drawRect(corner: {x = Int32, y = Int32}, dim: {width = Int32, height = Int32}, color: Color, d: Drawing): Unit \ IO = {
        let Drawing.Drawing(g) = d;
        setColorMode(color, g);
        g.fillRect(corner#x, corner#y, dim#width, dim#height)
    }

    pub def drawCircle(center: {x = Int32, y = Int32}, radius: {radius = Int32}, color: Color, d: Drawing): Unit \ IO = {
        let Drawing.Drawing(g) = d;
        setColorMode(color, g);
        let offset = radius#radius / 2;
        g.fillOval(center#x - offset, center#y - offset, radius#radius, radius#radius)
    }

    pub type alias Font = {
        size = Float32,
        color = Color,
        bold = Bool,
        italic = Bool
    }

    pub def drawText(corner: {x = Int32, y = Int32}, s: String, font: Font, d: Drawing): Unit \ IO = {
        let Drawing.Drawing(g) = d;
        setFontMode(font, g);
        g.drawString(s, corner#x, corner#y)
    }

    pub def drawTextCenter(center: {x = Int32, y = Int32}, s: String, font: Font, d: Drawing): Unit \ IO = {
        let Drawing.Drawing(g) = d;
        setFontMode(font, g);
        let metrics = g.getFontMetrics();
        let width = metrics.stringWidth(s);
        let height = metrics.getHeight();
        let x = center#x - width/2;
        let y = center#y + height/2;
        g.drawString(s, x, y)
    }

    type alias Frame = {
        frame = JFrame,
        waitForExit = Unit -> Unit \ Chan
    }

    def makeFrame(config: App[ef1, ef2, ef3, ef4, ef5, ef6, ef7]): Frame \ IO + Chan + ef1 = {
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
        let drawing = Drawing.Drawing(initG);
        drawRect({x = 0, y = 0}, {height = height, width = width}, Color.Black, drawing);
        init(drawing);
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

    enum ProxyEff[_: Eff] {
        case Proxy
    }

    def touchEff(_: ProxyEff[ef]): Unit \ ef = checked_ecast(())

    def addMouseListener(image: BufferedImage, listener: MouseListener[ef1, ef2, ef3, ef4, ef5, ef6], base: JComponent): Unit \ IO = {
        base.addMouseListener(new JMouseListener {
            def mouseExited(_this: JMouseListener, _event: MouseEvent): Unit = ()
            def mouseEntered(_this: JMouseListener, _event: MouseEvent): Unit = ()
            def mousePressed(_this: JMouseListener, event: MouseEvent): Unit \ ef1 + ef2 + IO = {
                let eventRec = {
                    x = event.getX(),
                    y = event.getY(),
                    but = getMouseButton(event)
                };
                listener#onPress(eventRec) |>
                    Option.forEach(paint -> paintImage(paint, base, image))
            }
            def mouseClicked(_this: JMouseListener, _event: MouseEvent): Unit = ()
            def mouseReleased(_this: JMouseListener, event: MouseEvent): Unit \ ef3 + ef4 + IO = {
                let eventRec = {
                    x = event.getX(),
                    y = event.getY(),
                    but = getMouseButton(event)
                };
                listener#onRelease(eventRec) |>
                    Option.forEach(paint -> paintImage(paint, base, image))
            }
        });
        base.addMouseMotionListener(new JMouseMotionListener {
            def mouseMoved(_this: JMouseMotionListener, _event: MouseEvent): Unit = ()
            def mouseDragged(_this: JMouseMotionListener, event: MouseEvent): Unit \ ef5 + ef6 + IO = {
                let eventRec = {
                    x = event.getX(),
                    y = event.getY()
                };
                listener#onDrag(eventRec) |>
                    Option.forEach(paint -> paintImage(paint, base, image))
            }
        })
    }

    def getMouseButton(event: MouseEvent): MouseButton =
        match unsafely IO run event.getButton() {
            case but if but == unsafely IO run MouseEvent.BUTTON1 => MouseButton.Left
            case but if but == unsafely IO run MouseEvent.BUTTON2 => MouseButton.Middle
            case but if but == unsafely IO run MouseEvent.BUTTON3 => MouseButton.Right
            case _ => MouseButton.Unknown
        }

    def paintImage(paint: Drawing -> Unit \ ef, base: JComponent, image: BufferedImage): Unit \ IO + ef = {
        let g = image.createGraphics();
        paint(Drawing.Drawing(g));
        g.dispose();
        base.repaint()
    }


    def setColorMode(color: Color, g: Graphics2D): Unit \ IO =
        match color {
            case Color.Black   => g.setColor(JColor.black)
            case Color.Blue    => g.setColor(JColor.blue)
            case Color.Cyan    => g.setColor(JColor.cyan)
            case Color.Gray    => g.setColor(JColor.gray)
            case Color.Green   => g.setColor(JColor.green)
            case Color.Magenta => g.setColor(JColor.magenta)
            case Color.Orange  => g.setColor(JColor.orange)
            case Color.Pink    => g.setColor(JColor.pink)
            case Color.Red     => g.setColor(JColor.red)
            case Color.White   => g.setColor(JColor.white)
            case Color.Yellow  => g.setColor(JColor.yellow)
        }

    def setFontMode(font: Font, g: Graphics2D): Unit \ IO = {
        let {size, color, bold, italic} = font;
        setColorMode(color, g);
        let g0 = g.getFont();
        let g1 = g0.deriveFont(size);
        let g2 = if (bold) g1.deriveFont(JFont.BOLD) else g1;
        let g3 = if (italic) g2.deriveFont(JFont.ITALIC) else g2;
        g.setFont(g3)
    }

}
