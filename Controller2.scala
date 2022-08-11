import javafx.fxml.FXML
import javafx.scene.control.{Button, MenuItem, Slider}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.Pane
import javafx.stage.FileChooser.ExtensionFilter
import javafx.stage.{FileChooser, Stage}

import java.io.File


class Controller2 extends Utils {
  private var qtree: QTree[Coords] = _
  private var qtreeOrigin: QTree[Coords] = _
  private var ImageName: String = _
  private var ImagesList: List[File] = _
  private var saved: Boolean = false
  private var savedImagePath: String = _
  private var contrastVal: Double = 0.5
  private var numberOfImages: Int = 0
  private var slide: Int = 0
  private var isSlideActive: Boolean = false


  //variaveis javafx
  @FXML
  private var im: ImageView = _

  @FXML
  private var rotateR: Button = _

  @FXML
  private var rotateRMenu: MenuItem = _

  @FXML
  private var save: MenuItem = _

  @FXML
  private var saveAs: MenuItem = _

  @FXML
  private var rotateL: Button = _

  @FXML
  private var rotateLMenu: MenuItem = _

  @FXML
  private var sepia: Button = _

  @FXML
  private var sepiaMenu: MenuItem = _

  @FXML
  private var contrasteFader: Slider = _

  @FXML
  private var contrastPMenu: MenuItem = _

  @FXML
  private var contrastP: Button = _

  @FXML
  private var contrastL: Button = _

  @FXML
  private var contrastLMenu: MenuItem = _

  @FXML
  private var resetB: Button = _

  @FXML
  private var resetBMenu: MenuItem = _

  @FXML
  private var mirrorH: Button = _

  @FXML
  private var mirrorV: Button = _

  @FXML
  private var slideshowpane: Pane = _

  @FXML
  private var editpane: Pane = _

  @FXML
  private var menuPane: Pane = _

  @FXML
  private def initialize(): Unit = {
    getImagesFromFolder

  }

  //TODO menuitem mirror v e h

  //funçoes
  def loadImage(): Unit = {

    val stage: Stage = new Stage()
    val fileChooser: FileChooser = new FileChooser
    fileChooser.getExtensionFilters.addAll(new ExtensionFilter("ImageType", "*.png"))
    val selectedFile: File = fileChooser.showOpenDialog(stage)
    im.setImage(new Image(selectedFile.toURI.toString))

    val imageData = ImageUtil.readColorImage(selectedFile.toPath.toString)
    qtree = BitMapToQtree(dataToBitMap(imageData))
    qtreeOrigin = qtree

    val filename: String = selectedFile.getName
    val newName = filename.split("\\.").head
    ImageName = newName + ".png"

    ImageUtil.writeImage(imageData, "Images\\" + newName + ".png", "png")
    getImagesFromFolder

  } //TODO Initial Menu

  def SaveAs = {
    val stage: Stage = new Stage()
    val fileChooser: FileChooser = new FileChooser
    fileChooser.setTitle("Save")
    fileChooser.getExtensionFilters.add(new ExtensionFilter("Imagem", "*.png"))
    fileChooser.getExtensionFilters.add(new ExtensionFilter("Imagem", "*.jpg"))

    val selectedFile: File = fileChooser.showSaveDialog(stage)
    val fileName: String = selectedFile.getName
    val extension: String = fileName.substring(fileName.lastIndexOf(".") + 1, fileName.length)
    val data = BitmapToData(QTreeToBitMap(qtree))

    ImageUtil.writeImage(data, selectedFile.getAbsolutePath, extension)
    saved = true
    savedImagePath = selectedFile.getAbsolutePath
  }

  def Save() = {
    if (saved) {
      val data = BitmapToData(QTreeToBitMap(qtree))
      val extension: String = savedImagePath.substring(savedImagePath.lastIndexOf(".") + 1, savedImagePath.length)
      ImageUtil.writeImage(data, savedImagePath, extension)
    }
    else {
      SaveAs
    }

  }

  def updateImage = {
    ImageUtil.writeImage(BitmapToData(QTreeToBitMap(qtree)), "Images/" + ImageName, "png")
    im.setImage(new Image("file:Images/" + ImageName))
  }

  def mirrorHView = {
    qtree = mirrorH(qtree)
    updateImage

  }

  def mirrorVView = {
    qtree = mirrorV(qtree)
    updateImage
  }

  def rotateRView = {
    qtree = rotateR(qtree)
    updateImage
  }

  def rotateLView = {
    qtree = rotateL(qtree)
    updateImage
  }

  def noiseView = {
    qtree = noiseImpure(qtree)
    updateImage
  }

  def sepiaView = {
    qtree = sepia(qtree)
    updateImage
  }

  def contrastUP = {
    if (contrastVal == 1) {}
    else {
      contrastVal = contrastVal + 0.05

      if (contrastVal > 1) {
        contrastVal = 1
      }
      qtree = defaultContrast(qtreeOrigin, contrastVal)
      updateImage
    }


  }

  def contrastDown = {
    if (contrastVal == 0) {}
    else {
      contrastVal = contrastVal - 0.05
      if (contrastVal < 0) {
        contrastVal = 0
      }
      qtree = defaultContrast(qtreeOrigin, contrastVal)
      updateImage
    }

  }

  def resetImage = {
    qtree = qtreeOrigin
    updateImage
  }

  def zoomin = {
    //qtree=scale(qtreeOrigin, zoom)
    updateImage
  }

  def zoomout = {

    //qtree=scale(qtreeOrigin, zoom)
    updateImage
  }

  //change views methods
  def getImagesFromFolder = {
    val file: File = new File("Images")
    ImagesList = file.listFiles.toList


  }

  def showSlideShow = {
    getImagesFromFolder
    actualisenumberOfimages
    if (numberOfImages == 0) {}
    else {
      slideshowpane.setVisible(true)
      editpane.setVisible(false)
      isSlideActive = true
      im.setImage(new Image(ImagesList(slide).toURI.toString))
    }
  }

  def nextImage = {
    if (isSlideActive) {
      if (slide == numberOfImages) {
        slide = 0
      }
      im.setImage(new Image(ImagesList(slide).toURI.toString))
      slide += 1
    }
  }

  def prevImage = {
    if (isSlideActive) {
      if (slide == 0) {
        slide = numberOfImages
      }
      im.setImage(new Image(ImagesList(slide).toURI.toString))

      slide -= 1
      if (slide <0){
        slide =0
      }
    }

  }


  def actualisenumberOfimages = {
    numberOfImages = ImagesList.length
  }

  def showEditMenu() = {
    editpane.setVisible(true)
    slideshowpane.setVisible(false)
    isSlideActive = false

  }

  def selectImagefromSlideShow = {
    showEditMenu
    hideSlideMenu
    val bitmap: Bitmap = dataToBitMap(ImageUtil.readColorImage(ImagesList(slide).toPath.toString))
    qtree = BitMapToQtree(bitmap)
    qtreeOrigin = qtree
  }

  def hideSlideMenu = {
    slideshowpane.setVisible(false)
  }

  def hideEditMenu = {
    editpane.setVisible(false)
  }
}