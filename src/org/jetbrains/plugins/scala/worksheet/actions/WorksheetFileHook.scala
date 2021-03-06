package org.jetbrains.plugins.scala
package worksheet.actions

import com.intellij.openapi.project.Project
import com.intellij.openapi.components.{ServiceManager, ProjectComponent}
import com.intellij.openapi.fileEditor._
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.openapi.editor.ex.EditorEx
import org.jetbrains.plugins.scala.worksheet.runconfiguration.WorksheetViewerInfo
import com.intellij.psi.PsiDocumentManager
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.worksheet.ui.{WorksheetFoldGroup, WorksheetEditorPrinter}
import org.jetbrains.plugins.scala.extensions
import javax.swing.JPanel
import java.awt.FlowLayout
import com.intellij.openapi.application.{ModalityState, ApplicationManager}
import org.jetbrains.plugins.scala.components.{WorksheetProcess, StopWorksheetAction}
import java.util
import java.lang.ref.WeakReference
import org.jetbrains.plugins.scala.worksheet.interactive.WorksheetAutoRunner

/**
 * User: Dmitry Naydanov
 * Date: 1/24/14
 */
class WorksheetFileHook(private val project: Project) extends ProjectComponent {
  override def disposeComponent() {}

  override def initComponent() {}

  override def projectClosed() {
    ApplicationManager.getApplication.invokeAndWait(new Runnable {
      def run() {
        WorksheetViewerInfo.invalidate()
      }
    }, ModalityState.any())
  }

  override def projectOpened() {
    project.getMessageBus.connect(project).subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, WorksheetEditorListener)
  }

  override def getComponentName: String = "Clean worksheet on editor close"

  def initTopComponent(file: VirtualFile, run: Boolean, exec: Option[WorksheetProcess] = None) {
    if (project.isDisposed) return

    val myFileEditorManager = FileEditorManager.getInstance(project)
    val editors = myFileEditorManager.getAllEditors(file)

    for (editor <- editors) {
      WorksheetFileHook.getAndRemovePanel(file) map {
        case ref =>
          val p = ref.get()

          ApplicationManager.getApplication.invokeLater(new Runnable {
            override def run() {
              if (p != null) myFileEditorManager.removeTopComponent(editor, p)
            }
          })
      }
      val panel = new WorksheetFileHook.MyPanel(file)

      panel.setLayout(new FlowLayout(FlowLayout.LEFT))


      extensions.inReadAction {
        new ChangeMakeAction(file, project).init(panel)
        new CopyWorksheetAction().init(panel)
        new CleanWorksheetAction().init(panel)
        if (run) new RunWorksheetAction().init(panel) else exec map (new StopWorksheetAction(_).init(panel))
      }

      myFileEditorManager.addTopComponent(editor, panel)
    }
  }

  def disableRun(file: VirtualFile, exec: Option[WorksheetProcess]) {
    cleanAndAdd(file, exec map (new StopWorksheetAction(_)))
  }

  def enableRun(file: VirtualFile) {
    cleanAndAdd(file, Some(new RunWorksheetAction))
  }

  private def cleanAndAdd(file: VirtualFile, action: Option[TopComponentAction]) {
    WorksheetFileHook getPanel file map {
      case panelRef =>
        val panel = panelRef.get()
        if (panel != null) {
          val c = panel getComponent 0
          if (c != null) panel remove c
          action map (_.init(panel))
        }
    }
  }

  private object WorksheetEditorListener extends FileEditorManagerListener {
    private def doc(source: FileEditorManager, file: VirtualFile) = source getSelectedEditor file match {
      case txtEditor: TextEditor if txtEditor.getEditor != null => txtEditor.getEditor.getDocument
      case _ => null
    }

    override def selectionChanged(event: FileEditorManagerEvent) {}

    override def fileClosed(source: FileEditorManager, file: VirtualFile) {
      if (ScalaFileType.WORKSHEET_EXTENSION == file.getExtension) {
        val d = doc(source, file)
        if (d != null) WorksheetAutoRunner.getInstance(source.getProject) removeListener d
      }
    }

    override def fileOpened(source: FileEditorManager, file: VirtualFile) {
      if (ScalaFileType.WORKSHEET_EXTENSION != file.getExtension) return

      WorksheetFileHook.this.initTopComponent(file, run = true)
      loadEvaluationResult(source, file)

      WorksheetAutoRunner.getInstance(source.getProject) addListener doc(source, file)
    }
    
    private def loadEvaluationResult(source: FileEditorManager, file: VirtualFile) {
      source getSelectedEditor file match {
        case txt: TextEditor => txt.getEditor match {
          case ext: EditorEx =>

            PsiDocumentManager getInstance project getPsiFile ext.getDocument match {
              case scalaFile: ScalaFile => WorksheetEditorPrinter.loadWorksheetEvaluation(scalaFile) foreach {
                case result if !result.isEmpty =>
                  val viewer = WorksheetEditorPrinter.createWorksheetViewer(ext, file, modelSync = true)
                  val document = viewer.getDocument

                  val splitter = WorksheetEditorPrinter.DIFF_SPLITTER_KEY.get(viewer)

                  extensions.inWriteAction {
                    document setText result
                    PsiDocumentManager.getInstance(project).commitDocument(document)

                    if (splitter != null) WorksheetFoldGroup.load(viewer, ext, project, splitter, scalaFile)
                  }
                case _ =>
              }
              case _ =>
            }
          case _ =>
        }
        case _ =>
      }
    }
    
  }
}

object WorksheetFileHook {
  private val file2panel = new util.WeakHashMap[VirtualFile, WeakReference[MyPanel]]()
  
  private class MyPanel(file: VirtualFile) extends JPanel {
    
    file2panel.put(file, new WeakReference[MyPanel](this))
    
    override def equals(obj: Any) = obj.isInstanceOf[MyPanel]

    override def hashCode() = Integer.MAX_VALUE
  }
  
  private def getAndRemovePanel(file: VirtualFile): Option[WeakReference[MyPanel]] = Option(file2panel.remove(file))

  private def getPanel(file: VirtualFile): Option[WeakReference[MyPanel]] = Option(file2panel get file)

  def instance(project: Project) = ServiceManager.getService(project, classOf[WorksheetFileHook])
}