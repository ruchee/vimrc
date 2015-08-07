" Vim syntax file
" Language:	C++
" Author: Dieter Hartmann
" License: GPL
"
" $Id: cpp.vim,v 1.2 2003/04/04 14:28:01 dihar Exp $
"
" -- Qt classes
"
"
syn keyword qClass QAccel QDict QIconFactory QPicture QSyntaxHighlighter
syn keyword qClass QAccessible QDictIterator QIconSet QPixmap Qt
syn keyword qClass QAccessibleInterface QDir QIconView QPixmapCache QTab
syn keyword qClass QAccessibleObject QDirectPainter QIconViewItem QPlatinumStyle QTabBar
syn keyword qClass QAction QDns QImage QPNGImagePacker QTabDialog
syn keyword qClass QActionGroup QDockArea QImageConsumer QPoint QTable
syn keyword qClass QApplication QDockWindow QImageDecoder QPointArray QTableItem
syn keyword qClass QAquaStyle QDomAttr QImageDrag QPopupMenu QTableSelection
syn keyword qClass QAsciiCache QDomCDATASection QImageFormat QPrinter QTabletEvent
syn keyword qClass QAsciiCacheIterator QDomCharacterData QImageFormatPlugin QProcess QTabWidget
syn keyword qClass QAsciiDict QDomComment QImageFormatType QProgressBar QTextBrowser
syn keyword qClass QAsciiDictIterator QDomDocument QImageIO QProgressDialog QTextCodec
syn keyword qClass QAssistantClient QDomDocumentFragment QIMEvent QPtrCollection QTextCodecPlugin
syn keyword qClass QAxAggregated  QDomDocumentType QInputDialog QPtrDict QTextDecoder
syn keyword qClass QAxBase  QDomElement QIntCache QPtrDictIterator QTextDrag
syn keyword qClass QAxBindable  QDomEntity QIntCacheIterator QPtrList QTextEdit
syn keyword qClass QAxFactory  QDomEntityReference QIntDict QPtrListIterator QTextEncoder
syn keyword qClass QAxObject  QDomImplementation QIntDictIterator QPtrQueue QTextIStream
syn keyword qClass QAxWidget  QDomNamedNodeMap QIntValidator QPtrStack QTextOStream
syn keyword qClass QBig5Codec QDomNode QIODevice QPtrVector QTextStream
syn keyword qClass QBig5hkscsCodec QDomNodeList QJisCodec QPushButton QThread
syn keyword qClass QBitArray QDomNotation QKbdDriverFactory QRadioButton QTime
syn keyword qClass QBitmap QDomProcessingInstruction QKbdDriverPlugin QRangeControl QTimeEdit
syn keyword qClass QBitVal QDomText QKeyEvent QRect QTimer
syn keyword qClass QBoxLayout QDoubleValidator QKeySequence QRegExp QTimerEvent
syn keyword qClass QBrush QDragEnterEvent QLabel QRegExpValidator QToolBar
syn keyword qClass QBuffer QDragLeaveEvent QLayout QRegion QToolButton
syn keyword qClass QButton QDragMoveEvent QLayoutItem QResizeEvent QToolTip
syn keyword qClass QButtonGroup QDragObject QLayoutIterator QScreen QToolTipGroup
syn keyword qClass QByteArray QDropEvent QLCDNumber QScrollBar QTranslator
syn keyword qClass QCache QEditorFactory QLibrary QScrollView QTranslatorMessage
syn keyword qClass QCacheIterator QErrorMessage QLineEdit QSemaphore QTsciiCodec
syn keyword qClass QCanvas QEucJpCodec QListBox QServerSocket QUriDrag
syn keyword qClass QCanvasEllipse QEucKrCodec QListBoxItem QSessionManager QUrl
syn keyword qClass QCanvasItem QEvent QListBoxPixmap QSettings QUrlInfo
syn keyword qClass QCanvasItemList QEventLoop QListBoxText QSGIStyle QUrlOperator
syn keyword qClass QCanvasLine QFile QListView QShowEvent QValidator
syn keyword qClass QCanvasPixmap QFileDialog QListViewItem QSignal QValueList
syn keyword qClass QCanvasPixmapArray QFileIconProvider QListViewItemIterator QSignalMapper QValueListConstIterator
syn keyword qClass QCanvasPolygon QFileInfo QLocalFs QSimpleRichText QValueListIterator
syn keyword qClass QCanvasPolygonalItem QFilePreview QMacStyle QSize QValueStack
syn keyword qClass QCanvasRectangle QFocusData QMainWindow QSizeGrip QValueVector
syn keyword qClass QCanvasSpline QFocusEvent QMap QSizePolicy QVariant
syn keyword qClass QCanvasSprite QFont QMapConstIterator QSjisCodec QVBox
syn keyword qClass QCanvasText QFontDatabase QMapIterator QSlider QVBoxLayout
syn keyword qClass QCanvasView QFontDialog QMemArray QSocket QVButtonGroup
syn keyword qClass QCDEStyle QFontInfo QMenuBar QSocketDevice QVGroupBox
syn keyword qClass QChar QFontManager QMenuData QSocketNotifier QWaitCondition
syn keyword qClass QCharRef QFontMetrics QMessageBox QSound QWhatsThis
syn keyword qClass QCheckBox QFrame QMetaObject QSpacerItem QWheelEvent
syn keyword qClass QCheckListItem QFtp QMetaProperty QSpinBox QWidget
syn keyword qClass QCheckTableItem QGb18030Codec QMimeSource QSplitter QWidgetFactory
syn keyword qClass QChildEvent QGb2312Codec QMimeSourceFactory QSql QWidgetItem
syn keyword qClass QClipboard QGbkCodec QMotif  QSqlCursor QWidgetPlugin
syn keyword qClass QCloseEvent QGfxDriverFactory QMotifDialog  QSqlDatabase QWidgetStack
syn keyword qClass QColor QGfxDriverPlugin QMotifPlusStyle QSqlDriver QWindowsMime
syn keyword qClass QColorDialog QGL QMotifStyle QSqlDriverPlugin QWindowsStyle
syn keyword qClass QColorDrag QGLayoutIterator QMotifWidget  QSqlEditorFactory QWizard
syn keyword qClass QColorGroup QGLColormap QMouseDriverFactory QSqlError QWMatrix
syn keyword qClass QComboBox QGLContext QMouseDriverPlugin QSqlField QWorkspace
syn keyword qClass QComboTableItem QGLFormat QMouseEvent QSqlFieldInfo QWSDecoration
syn keyword qClass QCommonStyle QGLWidget QMoveEvent QSqlForm QWSInputMethod
syn keyword qClass QConstString QGrid QMovie QSqlIndex QWSKeyboardHandler
syn keyword qClass QContextMenuEvent QGridLayout QMutex QSqlPropertyMap QWSMouseHandler
syn keyword qClass QCopChannel QGridView QMutexLocker QSqlQuery QWSServer
syn keyword qClass QCString QGroupBox QNetworkOperation QSqlRecord QWSWindow
syn keyword qClass QCursor QGuardedPtr QNetworkProtocol QSqlRecordInfo QXmlAttributes
syn keyword qClass QCustomEvent QHBox QNPInstance  QSqlResult QXmlContentHandler
syn keyword qClass QCustomMenuItem QHBoxLayout QNPlugin  QStatusBar QXmlDeclHandler
syn keyword qClass QDataBrowser QHButtonGroup QNPStream  QStoredDrag QXmlDefaultHandler
syn keyword qClass QDataStream QHeader QNPWidget  QStrIList QXmlDTDHandler
syn keyword qClass QDataTable QHebrewCodec QObject QString QXmlEntityResolver
syn keyword qClass QDataView QHGroupBox QObjectCleanupHandler QStringList QXmlErrorHandler
syn keyword qClass QDate QHideEvent QObjectList QStrList QXmlInputSource
syn keyword qClass QDateEdit QHostAddress QPaintDevice QStrListIterator QXmlLexicalHandler
syn keyword qClass QDateTime QHttp QPaintDeviceMetrics QStyle QXmlLocator
syn keyword qClass QDateTimeEdit QHttpHeader QPainter QStyleFactory QXmlNamespaceSupport
syn keyword qClass QDeepCopy QHttpRequestHeader QPaintEvent QStyleOption QXmlParseException
syn keyword qClass QDesktopWidget QHttpResponseHeader QPair QStylePlugin QXmlReader
syn keyword qClass QDial QIconDrag QPalette QStyleSheet QXmlSimpleReader
syn keyword qClass QDialog QIconDragItem QPen QStyleSheetItem QXtWidget 

" --- Qt keywords
"
syn keyword     cType           SIGNAL SLOT

" --- Qt Macros
"
syn keyword     cType           Q_ASSERT Q_CHECK_PTR Q_OBJECT

" highlight Qt classes like bulid-in cpp types
highlight link qClass Type

