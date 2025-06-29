mod Graphics {
    import java.awt.{Color => JColor}
    import java.awt.{Font => JFont}
    import java.awt.{Graphics => JGraphics}
    import java.awt.Dimension
    import java.awt.event.{KeyListener => JKeyListener}
    import java.awt.event.{MouseListener => JMouseListener}
    import java.awt.event.{MouseMotionListener => JMouseMotionListener}
    import java.awt.event.{MouseWheelListener => JMouseWheelListener}
    import java.awt.event.KeyEvent
    import java.awt.event.MouseEvent
    import java.awt.event.MouseWheelEvent
    import java.awt.event.WindowEvent
    import java.awt.event.WindowListener
    import java.awt.Graphics2D
    import java.awt.image.BufferedImage
    import java.awt.image.ImageObserver
    import javax.swing.JComponent
    import javax.swing.JFrame

    pub type alias App[ef: Eff] = {
        title = String,
        height = Int32,
        width = Int32,
        init = Drawing -> Unit \ ef,
        mouseListener = MouseListener[ef],
        keyListener = KeyListener[ef]
    }

    mod App {
        use Graphics.Drawing
        use Graphics.MouseListener
        use Graphics.KeyListener

        pub def mk(app: {
            title = String,
            height = Int32,
            width = Int32,
            init = Drawing -> Unit \ ef1,
            mouseListener = MouseListener[ef2],
            keyListener = KeyListener[ef3]
        }): App[ef1 + ef2 + ef3] = {
            unchecked_cast(app as App[ef1 + ef2 + ef3])
        }
    }

    pub type alias SubApp[ef: Eff] = {
        init = Drawing -> Unit \ ef,
        mouseListener = MouseListener[ef],
        keyListener = KeyListener[ef]
    }

    pub enum Drawing {
        case Drawing(Graphics2D)
    }

    pub enum Action[ef: Eff] {
        case Draw(Drawing -> Unit \ ef)
        case Run(SubApp[ef])
        case Nothing
    }

    mod Action {
        use Graphics.Drawing
        use Graphics.SubApp

        pub def draw(f: Drawing -> Unit \ ef): Action[ef + ef1] =
            unchecked_cast(Draw(f) as Action[ef + ef1])

        pub def runApp(app: SubApp[ef]): Action[ef + ef1] =
            unchecked_cast(Run(app) as Action[ef + ef1])
    }

    pub enum MouseButton with Eq, ToString {
        case Left
        case Middle
        case Right
        case Unknown
    }

    pub type alias MouseListener[ef: Eff] = {
        onPress = {x = Int32, y = Int32, but = MouseButton} -> Action[ef] \ ef,
        onRelease = {x = Int32, y = Int32, but = MouseButton} -> Action[ef] \ ef,
        onDrag = {x = Int32, y = Int32} -> Action[ef] \ ef,
        onScroll = {amount = Int32} -> Action[ef] \ ef
    }

    mod MouseListener {
        use Graphics.MouseButton
        use Graphics.Drawing
        use Graphics.Action

        pub def mk(listener: {
            onPress = {x = Int32, y = Int32, but = MouseButton} -> Action[ef1] \ ef2,
            onRelease = {x = Int32, y = Int32, but = MouseButton} -> Action[ef3] \ ef4,
            onDrag = {x = Int32, y = Int32} -> Action[ef5] \ ef6,
            onScroll = {amount = Int32} -> Action[ef7] \ ef8
        }): MouseListener[ef1 + ef2 + ef3 + ef4 + ef5 + ef6 + ef7 + ef8 + ef9] = {
            unchecked_cast(listener as MouseListener[ef1 + ef2 + ef3 + ef4 + ef5 + ef6 + ef7 + ef8 + ef9])
        }

        pub def empty(): MouseListener[ef] = mk({
            onPress = _ -> Action.Nothing,
            onRelease = _ -> Action.Nothing,
            onDrag = _ -> Action.Nothing,
            onScroll = _ -> Action.Nothing
        })
    }

    pub enum Key with Eq, ToString {
        case Letter(Char)
        case Enter
        case Unknown
    }

    pub type alias KeyListener[ef: Eff] = {
        onPress = Key -> Action[ef] \ ef
    }

    mod KeyListener {
        use Graphics.Action
        use Graphics.Key
        use Graphics.Drawing

        pub def mk(listener: {
            onPress = Key -> Action[ef1] \ ef2
        }): KeyListener[ef1 + ef2 + ef3] = {
            unchecked_cast(listener as KeyListener[ef1 + ef2 + ef3])
        }

        pub def empty(): KeyListener[ef] = mk({
            onPress = _ -> Action.Nothing
        })
    }

    pub def runApp(config: App[ef]): Unit \ IO + ef = {
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

    pub def drawOval(center: {x = Int32, y = Int32}, dim: {width = Int32, height = Int32}, color: Color, d: Drawing): Unit \ IO = {
        let Drawing.Drawing(g) = d;
        setColorMode(color, g);
        g.fillOval(center#x - dim#width/2, center#y - dim#height/2, dim#width, dim#height)
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

    type alias State[ef: Eff, r: Region] = {
        keyListener = Ref[KeyListener[ef], r],
        mouseListener = Ref[MouseListener[ef], r]
    }

    def makeFrame(config: App[ef]): Frame \ IO + Chan + ef = region rc {
        let {title, height, width, init, mouseListener = mouseListener0, keyListener = keyListener0} = config;
        let state = {
            mouseListener = Ref.fresh(rc, mouseListener0),
            keyListener = Ref.fresh(rc, keyListener0)
        };
        let (send, recv) = Channel.buffered(1);
        def waitForClose() = {
            // Unit only has one value, so no non-determinism.
            unsafely NonDet run Channel.recv(recv);
            Channel.send((), send)
        };
        let f = new JFrame(title);
        f.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        f |> addOnExitListener(() -> Channel.send((), send));
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
            def getPreferredSize(_this: JComponent): Dimension \ IO = {
                new Dimension(width, height)
            }
        };
        base |> addMouseListener(image, state);
        (base, f) ||> addKeyListener(image, state);
        f.add(base);
        f.pack();
        f.setLocationRelativeTo(unchecked_cast(null as JComponent));
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

    def addMouseListener(image: BufferedImage, state: State[ef, r], base: JComponent): Unit \ IO = {
        base.addMouseListener(new JMouseListener {
            def mouseExited(_this: JMouseListener, _event: MouseEvent): Unit = ()
            def mouseEntered(_this: JMouseListener, _event: MouseEvent): Unit = ()
            def mousePressed(_this: JMouseListener, event: MouseEvent): Unit \ ef + IO + r = {
                let eventRec = {
                    x = event.getX(),
                    y = event.getY(),
                    but = getMouseButton(event)
                };
                Ref.get(state#mouseListener)#onPress(eventRec) |> applyAction(base, image, state)
            }
            def mouseClicked(_this: JMouseListener, _event: MouseEvent): Unit = ()
            def mouseReleased(_this: JMouseListener, event: MouseEvent): Unit \ ef + IO + r = {
                let eventRec = {
                    x = event.getX(),
                    y = event.getY(),
                    but = getMouseButton(event)
                };
                Ref.get(state#mouseListener)#onRelease(eventRec) |> applyAction(base, image, state)
            }
        });
        base.addMouseMotionListener(new JMouseMotionListener {
            def mouseMoved(_this: JMouseMotionListener, _event: MouseEvent): Unit = ()
            def mouseDragged(_this: JMouseMotionListener, event: MouseEvent): Unit \ ef + IO + r = {
                let eventRec = {
                    x = event.getX(),
                    y = event.getY()
                };
                Ref.get(state#mouseListener)#onDrag(eventRec) |> applyAction(base, image, state)
            }
        });
        base.addMouseWheelListener(new JMouseWheelListener {
            def mouseWheelMoved(_this: JMouseWheelListener, event: MouseWheelEvent): Unit \ ef + IO + r = {
                Ref.get(state#mouseListener)#onScroll({amount = event.getWheelRotation()}) |> applyAction(base, image, state)
            }
        })
    }

    def applyAction(
        base: JComponent, image: BufferedImage, state: State[ef, r], action: Action[ef]
    ): Unit \ IO + ef + r = {
        match action {
            case Action.Draw(paint) =>
                let g = image.createGraphics();
                paint(Drawing.Drawing(g));
                g.dispose();
                base.repaint()
            case Action.Run(app) =>
                let g = image.createGraphics();
                app#init(Drawing.Drawing(g));
                g.dispose();
                base.repaint();
                state#keyListener |> Ref.put(app#keyListener);
                state#mouseListener |> Ref.put(app#mouseListener)
            case Action.Nothing =>
                ()
        }
    }

    def getMouseButton(event: MouseEvent): MouseButton =
        match unsafely IO run event.getButton() {
            case but if but == unsafely IO run MouseEvent.BUTTON1 => MouseButton.Left
            case but if but == unsafely IO run MouseEvent.BUTTON2 => MouseButton.Middle
            case but if but == unsafely IO run MouseEvent.BUTTON3 => MouseButton.Right
            case _ => MouseButton.Unknown
        }

    def addKeyListener(image: BufferedImage, state: State[ef, r], base: JComponent, frame: JFrame): Unit \ IO = {
        frame.addKeyListener(new JKeyListener {
            def keyPressed(_this: JKeyListener, event: KeyEvent): Unit \ IO + ef + r = {
                Ref.get(state#keyListener)#onPress(getKeyFromEvent(event)) |> applyAction(base, image, state)
            }
            def keyReleased(_this: JKeyListener, _event: KeyEvent): Unit = ()
            def keyTyped(_this: JKeyListener, _event: KeyEvent): Unit = ()
        })
    }

    def getKeyFromEvent(event: KeyEvent): Key = {
        let code = unsafely IO run event.getKeyCode();
        if (code == unsafely IO run KeyEvent.VK_A) Key.Letter('a')
        else if (code == unsafely IO run KeyEvent.VK_B) Key.Letter('b')
        else if (code == unsafely IO run KeyEvent.VK_C) Key.Letter('c')
        else if (code == unsafely IO run KeyEvent.VK_D) Key.Letter('d')
        else if (code == unsafely IO run KeyEvent.VK_E) Key.Letter('e')
        else if (code == unsafely IO run KeyEvent.VK_F) Key.Letter('f')
        else if (code == unsafely IO run KeyEvent.VK_G) Key.Letter('g')
        else if (code == unsafely IO run KeyEvent.VK_H) Key.Letter('h')
        else if (code == unsafely IO run KeyEvent.VK_I) Key.Letter('i')
        else if (code == unsafely IO run KeyEvent.VK_J) Key.Letter('j')
        else if (code == unsafely IO run KeyEvent.VK_K) Key.Letter('k')
        else if (code == unsafely IO run KeyEvent.VK_L) Key.Letter('l')
        else if (code == unsafely IO run KeyEvent.VK_M) Key.Letter('m')
        else if (code == unsafely IO run KeyEvent.VK_N) Key.Letter('n')
        else if (code == unsafely IO run KeyEvent.VK_O) Key.Letter('o')
        else if (code == unsafely IO run KeyEvent.VK_P) Key.Letter('p')
        else if (code == unsafely IO run KeyEvent.VK_Q) Key.Letter('q')
        else if (code == unsafely IO run KeyEvent.VK_R) Key.Letter('r')
        else if (code == unsafely IO run KeyEvent.VK_S) Key.Letter('x')
        else if (code == unsafely IO run KeyEvent.VK_T) Key.Letter('t')
        else if (code == unsafely IO run KeyEvent.VK_U) Key.Letter('u')
        else if (code == unsafely IO run KeyEvent.VK_V) Key.Letter('v')
        else if (code == unsafely IO run KeyEvent.VK_W) Key.Letter('w')
        else if (code == unsafely IO run KeyEvent.VK_X) Key.Letter('x')
        else if (code == unsafely IO run KeyEvent.VK_Y) Key.Letter('y')
        else if (code == unsafely IO run KeyEvent.VK_Z) Key.Letter('z')
        else if (code == unsafely IO run KeyEvent.VK_ENTER) Key.Enter
        else Key.Unknown
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
