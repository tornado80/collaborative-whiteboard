import './App.css'
import * as React from "react"
import Modal from "react-modal"
import ToolButton from "./ToolButton"
import { mdiPencil, mdiEraser, mdiNote, mdiComment, mdiImage, mdiCursorMove, mdiCursorDefault } from "@mdi/js"
import html2canvas from "html2canvas"
import { Session } from './state/session';

// Define constants
const ERASER_SIZE = 8 // diameter in px (circle or square)
const WAYPOINT_MIN_DIST = ERASER_SIZE / 2 // Minimum distance between consequtive waypoints

const BOARD_WIDTH = 1920
const BOARD_HEIGHT = 1080

const BOARD_MARGIN = 8 /* Also defined in css (if you change, change it there as well) */

// Describe tool states (as enum as JS can provide ;))
const Tool = {
  Default: "default",
  Move: "move",
  Draw: "draw",
  Erase: "erase",
  StickyNote: "note",
  Image: "image",
  Comment: "comment",
}

function dataURItoBlob(uri) {
  const bs = atob(uri.split(',')[1]);
  var mime = uri.split(',')[0].split(':')[1].split(';')[0]
  var ab = new ArrayBuffer(bs.length);

  // create a view into the buffer
  var ia = new Uint8Array(ab);

  // set the bytes of the buffer to the correct values
  for (var i = 0; i < bs.length; i++) {
      ia[i] = bs.charCodeAt(i);
  }
  return new Blob([ab], {type: mime});

}

export default class App extends React.Component {

  state = {
    state: {}, // Board state
    selectedTool: Tool.Default,
    // Most recent mouse state
    mouse: {
      x: 0, // Latest registered x (translated to canvas coordinates)
      y: 0, // Latest registered x (translated to canvas coordinates)
      down: false, // Is the mous button down
      dragStartX: 0, // Position x where mouse was pressed down (translated to canvas coordinates)
      dragStartY: 0, // Position y where mouse was pressed down (translated to canvas coordinates)
    },
    actionPos: null, // Click position as {x: 0, y: 0} where the action was started (translated to canvas coordinates)
    commentModalOpen: false, // Is the comment modal currently opened
    noteModalOpen: false, // Is the sticky note modal currently opened
    imageModalOpen: false, // Is the image modal currently opened
    imgToUpload: "", // Image blob to be uploaded (URL-formatted blob)
    stickyNoteText: "", // Sticky note text to be set
    commentText: "", // Comment text to be set
  }

  curveWaypoints = [] // Waypoints of current path to be drawn or erased

  constructor(props) {
    super(props)

    this.canvasRef = React.createRef(null)
    this.canvasScrollRef = React.createRef(null)
  }

  componentDidMount() {
    if (!this.session) {
      this.session = new Session(() => this.state.state, (state) => this.setState({...this.state, state}))
    }
  }

  // Set current tool to another
  switchTool(tool) {
    this.setState({
      ...this.state,
      selectedTool: tool,
    })
  }

  // Event handler for mouse click events on canvas
  onMouseClickOnCanvas(event) {
    const scroll = this.canvasScrollRef.current

    this.clickOnCanvas({
      x: event.clientX - event.target.offsetLeft + scroll.scrollLeft,
      y: event.clientY - event.target.offsetTop + scroll.scrollTop,
    })
  }
  
  // Event handler for mouse move events on canvas
  onMouseMoveOnCanvas(event) {
    const scroll = this.canvasScrollRef.current
    const canvas = this.canvasRef.current

    const x = Math.min( Math.max(event.clientX - canvas.offsetLeft + scroll.scrollLeft, 0), BOARD_WIDTH)
    const y = Math.min( Math.max(event.clientY - canvas.offsetTop - scroll.offsetTop + scroll.scrollTop, 0), BOARD_HEIGHT)

    this.registerMoveTo(x, y)

    if (this.state.mouse.down) {
      this.dragOnCanvas()
    }
  }

  registerMoveTo(x, y, down) {
    let curveWaypoints = []
    if (this.state.mouse.down && [Tool.Draw, Tool.Erase].includes(this.state.selectedTool)) {

      // Linearly re-create the waypoints to keep the minimum distance between subsequent waypoints
      // TODO: Could be updated to use bspline for fancier user experience
      if (this.curveWaypoints.length) {
        const lastX = this.curveWaypoints[this.curveWaypoints.length-1].x
        const lastY = this.curveWaypoints[this.curveWaypoints.length-1].y

        const dist = Math.sqrt(Math.pow(x - lastX, 2) + Math.pow(y - lastY, 2))
        const targetNum = Math.ceil(dist / WAYPOINT_MIN_DIST)

        for (let i = 0; i < targetNum; i++) {
          const extrapolated = {x: Math.round(lastX + (x - lastX) / targetNum * i), y: Math.round(lastY + (y - lastY) / targetNum * i)}
          curveWaypoints.push(extrapolated)
        }
      }

      curveWaypoints.push({x, y})
      this.addWaypointsToCurve(curveWaypoints)
    }

    if (this.state.mouse.down && this.state.selectedTool === Tool.Move) {
      this.scrollCanvas(this.state.mouse.x -x, this.state.mouse.y - y)
    }

    this.setState({
      ...this.state,
      mouse: {
        ...this.state.mouse,
        x,
        y,
        down: down === undefined ? this.state.mouse.down : down,
      },
    })
  }
  
  // Event handler for mouse down events on canvas
  onMouseDownOnCanvas(event) {
    this.setState({
      ...this.state,
      mouse: {
        ...this.state.mouse,
        down: true,
        dragStartX: event.offsetX,
        dragStartY: event.offsetY,
      }
    })

    this.dragOnCanvas()
  }

  // Event handler for mouse up events on canvas
  onMouseUpOnCanvas(event) {
    this.setState({
      ...this.state,
      mouse: {
        ...this.state.mouse,
        down: false,
      }
    })
  }

  // Event handler for mouseout events on canvas
  onMouseOutFromCanvas(event) {
    const scroll = this.canvasScrollRef.current
    const canvas = this.canvasRef.current

    const x = event.clientX - canvas.offsetLeft + scroll.scrollLeft
    const y = event.clientY - canvas.offsetTop - scroll.offsetTop + scroll.scrollTop

    if (x < 0 || x > BOARD_WIDTH || y < 0 || y > BOARD_HEIGHT) {
      const lastX = this.state.mouse.x
      const lastY = this.state.mouse.y

      /* Calculate the end for the stroke, i.e. where did the mouse crossed the edge of the canvas */
      const s = [lastX, lastY]
      const r = [x - lastX, y - lastY]
      const bMin = [scroll.scrollLeft - BOARD_MARGIN, scroll.scrollTop - BOARD_MARGIN]
      const bMax = [Math.min(scroll.scrollLeft + scroll.clientWidth - BOARD_MARGIN, BOARD_WIDTH), Math.min(scroll.scrollTop + scroll.clientHeight - BOARD_MARGIN, BOARD_HEIGHT)]
      
      let ts = -Infinity
      let te = Infinity

      for (let i = 0; i < 2; i++) {
        let t1 = (bMin[i] - s[i]) / r[i]
        let t2 = (bMax[i] - s[i]) / r[i]

        if (t1 > t2) {
          [t1, t2] = [t2, t1]
        }

        ts = Math.max(ts, t1)
        te = Math.min(te, t2)
      }
      
      // Round to use integer coordinates (also works for floating points without rounding, though)
      const eX = Math.round(s[0] + te * r[0])
      const eY = Math.round(s[1] + te * r[1])

      const wasDown = this.state.mouse.down

      // Register the last part of the movement till the edge of the cnvas
      this.registerMoveTo(eX, eY, false)

      if (wasDown) {
        this.clickOnCanvas()
      }
    }
  }

  // Close all modals & discard all non-saved values from prompts
  resetActionState() {
    this.setState({
      ...this.state,
      commentModalOpen: false,
      noteModalOpen: false,
      imageModalOpen: false,
      imgToUpload: "",
      stickyNoteText: "",
      commentText: "",
    })

    this.curveWaypoints = []
  }

  // Handler for image change event on image modal
  onImgChange(event) {
    const r = new FileReader()

    r.onloadend = () => {
      this.setState({
        ...this.state,
        imgToUpload: r.result
      })
    }

    const file = event.target.files[0]

    if (file) {
      r.readAsDataURL(file)
    } else {
      this.setState({
        ...this.state,
        imgToUpload: ""
      })
    }
  }

  // Scrolls the canvas with given diff
  scrollCanvas(x, y) {
    const scroll = this.canvasScrollRef.current

    scroll.scrollTo(scroll.scrollLeft + x, scroll.scrollTop + y)
  }


  /* Higher level functions to control the application logic */

  // Triggers actions based on selected tool
  clickOnCanvas(pos) {
    // This method is called when user clicks and immediately releases the mouse on canvas

    if (this.state.selectedTool === Tool.Comment) {
      this.setState({
        ...this.state,
        commentModalOpen: true,
        actionPos: pos,
      })
    } else if (this.state.selectedTool === Tool.Image) {
      this.setState({
        ...this.state,
        imageModalOpen: true,
        actionPos: pos,
      })
    } else if (this.state.selectedTool === Tool.StickyNote) {
      this.setState({
        ...this.state,
        noteModalOpen: true,
        actionPos: pos,
      })
    } else if (this.state.selectedTool === Tool.Erase) {
      this.setState({
        ...this.state,
        actionPos: pos,
      })
      this.erase()
    } else if (this.state.selectedTool === Tool.Draw) {
      this.setState({
        ...this.state,
        actionPos: pos,
      })
      this.drawCurve()
    }
  }

  // Triggers actions based on selected tool
  dragOnCanvas() {
    // This method is called when the user drags the mouse on canvas (moving while mouse is down)
    // This will be called every time the mouse position is updated while the mouse was down.

    if ([Tool.Draw, Tool.Erase].includes(this.state.selectedTool)) {
      this.reRenderCanvas()
    }
  }

  /* Tool-specific methods */

  // Called when modal "form" is submitted and changes the board state.
  addStickyNote() {    
    this.session.proposeUpdate({
      canvasObjectType: "stickyNote",
      operationType: "create",
      operation: {
        canvasObjectOperationType: "createStickyNote",
        position: { x: this.state.actionPos.x, y: this.state.actionPos.y },
        text: this.state.stickyNoteText,
        color: "#ffd500"
      }
    })
  
    // Reset the action state after sending the payload
    this.resetActionState();
  }

  // Called when modal "form" is submitted and changes the board state.
  addImage() {
    //this.state.actionPos.x
    //this.state.actionPos.y
    //this.state.imgToUpload

    // Wait until action gets confirmed
    // Alert if proposal was not successful

    // TODO: Upload blob & include it into update proposal

    this.session.sendBlob(dataURItoBlob(this.state.imgToUpload)).then(blobId => {
      this.session.proposeUpdate({
        canvasObjectType: "image",
        operationType: "create",
        operation: {
          canvasObjectOperationType: "createImage",
          position: { x: this.state.actionPos.x, y: this.state.actionPos.y },
          blobId: blobId,
          width: 100,
        }
      })

      // Reset the action state after sending the payload
      this.resetActionState();
    })
  }

  // Called when modal "form" is submitted and changes the board state.
  addComment() {
    //this.state.actionPos.x
    //this.state.actionPos.y
    //this.state.commentText

    // Wait until action gets confirmed
    // Alert if proposal was not successful
    this.session.proposeUpdate({
      canvasObjectType: "comment",
      operationType: "create",
      operation: {
        canvasObjectOperationType: "createComment",
        text: this.state.commentText,
        timestamp: new Date().toISOString(),
      }
    })
    
    // Reset the action state after sending the payload
    this.resetActionState();
  }

  removeObject(objectId) {
    // TODO: Fix
    this.session.proposeUpdate({
      //canvasObjectType: "stickyNote",
      operationType: "draw",
      operation: {
        canvasObjectOperationType: "draw",
        points: [...this.curveWaypoints],
        color: "#000000", // black
      }
    })
  }

  // Called when drag finished and changes the board state.
  drawCurve() {
    // Wait until action gets confirmed
    // Alert if proposal was not successful
    this.session.proposeUpdate({
      canvasObjectType: "canvas",
      operationType: "draw",
      operation: {
        canvasObjectOperationType: "draw",
        points: [...this.curveWaypoints],
        color: "#000000", // black
      }
    })
  
    // Reset the action state after sending the payload
    this.resetActionState();
  }

  // Called when drag finished and changes the board state.
  erase() {
    // Wait until action gets confirmed
    // Alert if proposal was not successful
    this.session.proposeUpdate({
      canvasObjectType: "canvas",
      operationType: "erase",
      operation: {
        canvasObjectOperationType: "erase",
        radius: ERASER_SIZE / 2,
        centers: [...this.curveWaypoints],
      }
    })
  
    // Reset the action state after sending the payload
    this.resetActionState();
  }

  /* Canvas rendereing */
  addWaypointsToCurve(waypoints) {
    const lastWaypoint = this.curveWaypoints[this.curveWaypoints.length - 1]
    this.curveWaypoints.push(...waypoints)

    const canvas = this.canvasRef.current
    if (!canvas) { return }

    const ctx = canvas.getContext("2d")

    // Render the new waypoints
    if (this.state.selectedTool === Tool.Draw) {
      for (const {x, y} of this.curveWaypoints) {
        ctx.beginPath()
        ctx.arc(x, y, 5, 0, 2 * Math.PI, false)
        ctx.fill()
      }
    } else if (this.state.selectedTool === Tool.Erase) {
      for (const {x, y} of this.curveWaypoints) {
        ctx.beginPath()
        ctx.clearRect(x, y, ERASER_SIZE, ERASER_SIZE)
        ctx.fill()
      }
    }
  }

  componentDidUpdate() {
    this.reRenderCanvas()
  }

  // Re-renders drawing layer on canvas
  reRenderCanvas() {
    const canvas = this.canvasRef.current
    if (!canvas) { return }

    // Clear canvas
    const ctx = canvas.getContext("2d")
    ctx.clearRect(0, 0, BOARD_WIDTH, BOARD_HEIGHT)

    // Render curve paths from boardState
    // Render from state
    this.state.state?.objects?.forEach(o => {
      if (o.canvasObjectType === "canvas") {
        if (o.operationType === "draw") {
          o.operation.points.forEach(p => {
            ctx.beginPath()
            ctx.arc(p.x, p.y, 5, 0, 2 * Math.PI, false)
            ctx.fill()
          })
        } else if (o.operationType === "erase") {
          o.operation.centers.forEach(p => {
            ctx.beginPath()
            ctx.clearRect(p.x, p.y, ERASER_SIZE, ERASER_SIZE)
            ctx.fill()
          })
        }
      } else if (o.canvasObjectType === "DrawCurve") {
        o.points.forEach(p => {
          ctx.beginPath()
          ctx.arc(p.x, p.y, 5, 0, 2 * Math.PI, false)
          ctx.fill()
        })
      } else if (o.canvasObjectType === "EraseCurve") {
        o.centers.forEach(p => {
          ctx.beginPath()
          ctx.clearRect(p.x, p.y, ERASER_SIZE, ERASER_SIZE)
          ctx.fill()
        })
      }
      
    })

    // Render current waypoints if erase or draw tool selected
    if (this.state.selectedTool === Tool.Draw) {
      for (const {x, y} of this.curveWaypoints) {
        ctx.beginPath()
        ctx.arc(x, y, 5, 0, 2 * Math.PI, false)
        ctx.fill()
      }
    } else if (this.state.selectedTool === Tool.Erase) {
      for (const {x, y} of this.curveWaypoints) {
        ctx.beginPath()
        ctx.clearRect(x, y, ERASER_SIZE, ERASER_SIZE)
        ctx.fill()
      }
    }
  }

  // export board
  exportAsPNG = () => {
    const element = document.getElementById('canvas'); 
    html2canvas(element).then(canvas => {
      const link = document.createElement('a');
      link.download = 'whiteboard.png';
      link.href = canvas.toDataURL();
      link.click();
    });
  };

  render() {
    return (
      <div className="app" id="whiteboard">
        <div className="toolbar">
          <ToolButton
            selected={this.state.selectedTool === Tool.Default}
            onClick={() => this.switchTool(Tool.Default)}
            icon={mdiCursorDefault}
          />
          <ToolButton
            selected={this.state.selectedTool === Tool.Move}
            onClick={() => this.switchTool(Tool.Move)}
            icon={mdiCursorMove}
          />
          <ToolButton
            selected={this.state.selectedTool === Tool.Draw}
            onClick={() => this.switchTool(Tool.Draw)}
            icon={mdiPencil}
          />
          <ToolButton
            selected={this.state.selectedTool === Tool.Erase}
            onClick={() => this.switchTool(Tool.Erase)}
            icon={mdiEraser}
          />
          <ToolButton
            selected={this.state.selectedTool === Tool.StickyNote}
            onClick={() => this.switchTool(Tool.StickyNote)}
            icon={mdiNote}
          />
          <ToolButton
            selected={this.state.selectedTool === Tool.Image}
            onClick={() => this.switchTool(Tool.Image)}
            icon={mdiImage}
          />
          <ToolButton
            selected={this.state.selectedTool === Tool.Comment}
            onClick={() => this.switchTool(Tool.Comment)}
            icon={mdiComment}
          />
          {/* x: {this.state.mouse.x},
          y: {this.state.mouse.y},
          down: {this.state.mouse.down ? "yes" : "no"} */}
        </div>
        <Modal
          isOpen={this.state.commentModalOpen}
        >
          <div className='modalbox'>
            <h1>Add comment</h1>
            <textarea value={this.state.commentText} onChange={e => this.setState({...this.state, commentText: e.target.value})}/>
            <div className='modalbuttons'>
              <button onClick={() => this.resetActionState()}>Cancel</button>
              <button onClick={() => this.addComment()}>Add</button>
            </div>
          </div>
        </Modal>
        <Modal
          isOpen={this.state.imageModalOpen}
        >
          <div className='modalbox'>
            <h1>Upload image</h1>
            <input id="img-input" type="file" name="file" accept=".jpg,.jpeg,.JPG,.JPEG" onChange={this.onImgChange.bind(this)} />
            <img src={this.state.imgToUpload} height="100vh"/>
            <div className='modalbuttons'>
              <button onClick={() => this.resetActionState()}>Cancel</button>
              <button onClick={() => this.addImage()}>Upload</button>
            </div>
          </div>
        </Modal>
        <Modal
          isOpen={this.state.noteModalOpen}
        >
          <div className='modalbox'>
            <h1>Add Sticky Note</h1>
            <textarea value={this.state.stickyNoteText} onChange={e => this.setState({...this.state, stickyNoteText: e.target.value})}/>
            <div className='modalbuttons'>
              <button onClick={() => this.resetActionState()}>Cancel</button>
              <button onClick={() => this.addStickyNote()}>Create</button>
            </div>
          </div>
        </Modal>
        <div className="canvas" id="canvas" ref={this.canvasScrollRef} style={{position: 'relative'}}
          onMouseMove={this.onMouseMoveOnCanvas.bind(this)}
          onMouseDown={this.onMouseDownOnCanvas.bind(this)}
          onMouseUp={this.onMouseUpOnCanvas.bind(this)}
          onClick={this.onMouseClickOnCanvas.bind(this)}
          onMouseOut={this.onMouseOutFromCanvas.bind(this)}
        >
          {/* TODO: For object in this.state?.objects */}
          { this.state.state?.objects?.map(obj => {
            const o = obj.operation ? obj.operation : obj
            const id = obj.canvasObjectId ? obj.canvasObjectId : obj.id
            switch (obj.canvasObjectType) {
              case "stickyNote":
              case "StickyNote":
                return  <div key={id} className="board-obj sticky-note" style={{position: 'absolute', top: o.position.y - this.canvasScrollRef.current.offsetTop + "px", left: o.position.x + this.canvasRef.current.offsetLeft + "px"}}>
                          { o.text }
                        </div>
              case "image":
              case "Image":
                return  <img key={id} className="board-obj image" style={{position: 'absolute', top: o.position.y - this.canvasScrollRef.current.offsetTop + "px", left: o.position.x + this.canvasRef.current.offsetLeft + "px"}} src={ this.session.getBlobResourceUrl(o.blobId) } />
              default:
                // Undefined object type (=> not rendered)
            }
          }) }
          <canvas
            className={`canvas-frame tool-${this.state.selectedTool}`}
            ref={this.canvasRef}
            width={BOARD_WIDTH}
            height={BOARD_HEIGHT}
          />
        </div>
        <div>
          <button style={{
            position: 'fixed',
            bottom: '20px',
            right: '20px',
            zIndex: '999',
            padding: '8px 16px',
            backgroundColor: '#f0f0f0',
            border: '1px solid #ccc',
            borderRadius: '4px',
            cursor: 'pointer',
          }} 
          onClick={this.exportAsPNG}>Export Board as PNG</button>
        </div>
      </div>
    );
  }
}
