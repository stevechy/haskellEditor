import Graphics.UI.Gtk
import Configuration.Types
import Data.Tree

main :: IO ()
main = do
    initGUI
    window <- windowNew
    set window [windowDefaultWidth := 800, windowDefaultHeight := 600]
    
    mainBox <- vBoxNew False 0
    _ <- containerAdd window mainBox

    button <- buttonNewWithLabel "Open"

    onClicked button newFileChooser

    boxPackStart mainBox button PackNatural 0
    
    mainHBox <- hBoxNew False 0
    fileTreeScrolledWindow <- scrolledWindowNew Nothing Nothing
    treeStore <- treeStoreNew [Node { rootLabel = "Hi", subForest = [] }]
    let column = makeColumnIdString 0
    treeModelSetColumn treeStore column id
    fileTreeView <- treeViewNewWithModel treeStore
    treeViewColumn <- treeViewColumnNew
    treeViewColumnSetTitle treeViewColumn "File"
    cellRenderer <- cellRendererTextNew
    treeViewColumnPackStart treeViewColumn cellRenderer True 
    cellLayoutSetAttributes treeViewColumn cellRenderer treeStore $ (\label -> [cellText := label])
    treeViewAppendColumn fileTreeView treeViewColumn
    Just maybeColumn <- treeViewGetColumn fileTreeView 0

    containerAdd fileTreeScrolledWindow fileTreeView
    widgetShowAll fileTreeView
    widgetShowAll fileTreeScrolledWindow
    boxPackStart mainHBox fileTreeScrolledWindow PackGrow 0
    
    noteBook <- notebookNew
    textViewScrolledWindow <- scrolledWindowNew Nothing Nothing
    textView <- textViewNew
    containerAdd textViewScrolledWindow textView
    notebookAppendPage noteBook textViewScrolledWindow "Tab"
    boxPackStart mainHBox noteBook PackGrow 0
    
    boxPackStart mainBox mainHBox PackGrow 0

    onDestroy window mainQuit
    widgetShowAll button    
    widgetShowAll mainBox
    widgetShowAll window
    mainGUI

newFileChooser = do
    window <- windowNew
    set window [windowDefaultWidth := 800, windowDefaultHeight := 600]

    fch <- fileChooserWidgetNew FileChooserActionOpen

    containerAdd window fch

    onFileActivated fch $
        do filePath <- fileChooserGetFilename fch
           case filePath of
               Just dpath -> do putStrLn dpath
                                widgetDestroy window
               Nothing -> return ()
     
    widgetShowAll fch
    widgetShowAll window
    return ()

