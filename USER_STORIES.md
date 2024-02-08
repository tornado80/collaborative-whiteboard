# User stories
This document contains all planned features and explanations how users are going to use them.

> All user actions on the canvas are saved automatically in the server.

## Whiteboard administration

### Creating a whiteboard
Actors: User

Steps: 
1. Click a button to create a session.

Post-condition:
User is shown a new whiteboard with the session ID.

### Open an existing whiteboard / Join whiteboard session
Actors: User

Steps: 
1. User clicks the "Join Sesion" button.
2. User enters the whiteboard session ID.
3. User clicks the Join button.

Error:
- If the session ID is invalid, the user is shown a proper error message that session ID is not valid. Then the user is returned to step 2.

Post-condition:
User is shown the whiteboard.

### Sharing a whiteboard
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps: 
1. User clicks the "Share" button.
2. User can see the whiteboard session ID.
3. User can copy the session ID and send it to whoever they want.

Post-condition:
Information is shared :D

## General features for whiteboards

### Undo last action
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. Clicking on Undo button or pressing Ctrl+Z

Post-condition: The last action of this specific user is undoed.

> Each user have their own undo history and history is preserved only for the current session

> Once the session is closed, the Undo/Redo history is deleted

### Redo an action
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. Clicking on Redo button or pressing Ctrl+Y, Ctrl+Shift+Z

Post-condition: The previosuly undoed action is redoed.

> Each user have their own undo history and history is preserved only for the current session

> Once the session is closed, the Undo/Redo history is deleted

### List of active users of the (current) whiteboard
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User can see the participants in the up right corner of the whiteboard

### See actions made by other users in real time
Actors: User A, User B

Pre-conditions: User A and User B both have joined the same session

Steps:
1. User A modifies the whiteboard by drawing or writing some text or comments
2. User B immediately notices User A's changes

## Drawing feature

### Draw on a whiteboard
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User selects pen tool by clicking the pen button on the toolbar.
2. User moves their pointer on the canvas while pressing the mouse left button to draw

Post-condition: User can see their artistic masterpiece :D

> Other users should see the drawing immediately, i.e. they can see the trace of drawer even before the drawer has finished their stroke. 

### Erase drawing on a whiteboard
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User selected eraser tool by clicking the eraser button on the toolbar.
2. User moves their point on the canvas while pressing mouse left button to erase

Post-condition: User can be relieved with their not so artistic masterpiece disappears :D

> Other users should see the erasing immediately. (They can optionally see the trace of erasing)

## Sticky notes feature

### Add a sticky note
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User selects sticky notes tool by clicking the sticky note button on the toolbar.
2. User clicks somewhere on the canvas.
3. User is prompted for text to be written on the sticky note.
4. User clicks the OK button to confirm.

Post-condition: User observes the sticky note with their chosen text on it.

> If the user clicks outside the prompt dialog, the action is cancelled.

### Remove a sticky note
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User selects hand tool by clicking the hand button on the toolbar.
2. User selects the stcky note by clicking on it.
3. Once the sticky note is selected, the user can delete it with Del button the keyboard.

Post-condition: User observes the sticky note disappears.

### Edit text on a sticky note
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User selects hand tool by clicking the hand button on the toolbar.
2. User double click on the sticky note.
3. User is prompted for the new text. (text entry is prefilled with the current text)
4. User clicks the OK button to confirm.

Post-condition: User observes the sticky note disappears.

> If the user clicks outside the prompt dialog, the action is cancelled, i.e. the text of sticky note is not changed.

### Moving sticky notes
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User selects hand tool by clicking the hand button on the toolbar.
2. User starts dragging the sticky note. (the sticky note is selected and user observes the selection)
3. User releases the sticky note on the place they want on the canvas.

Post-condition: Sticky note is not in the same place that is used to be :D

> Other users can observe the moving immediately, i.e. they should see the trace of moving even before the mover has not finished with the placement.

## Image feature

### Upload an image (to add it on the board)
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User selects image tool by clicking the image button on the toolbar.
2. User is prompted with the file dialog to choose the image from their computer
3. User observes image is uploading
4. User observes image is uplaoded and placed on the center of the canvas.

Post-condition: An image is added to the canvas.

### Remove an image
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User selects hand tool by clicking the hand button on the toolbar.
2. User selects the image by clicking on it.
3. Once the image is selected (with a significant border), the user can delete it with Del button on the keyboard.

Post-condition: User observes the image disappears.

### Move an image
Actors: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User selects hand tool by clicking the hand button on the toolbar.
2. User starts dragging the image. (the image is selected and user observes the selection)
3. User releases the image on the place they want on the canvas.

Post-condition: Image is not in the same place that is used to be :D

> Other users can observe the moving immediately, i.e. they should see the trace of moving even before the mover has not finished with the placement.

### Comment on a canvas object
Actor: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User selects the comment tool by clicking the comment button on the toolbar.
2. User clicks on a canvas object
3. User is prompted for comment text
4. User enters the text and clicks on comment button

Post-condition: A comment icon appears on the object.

> Comment should get automatically removed when the parent image is removed

### View comment on the canvas
Actor: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User clicks on the comment icon visible on the canvas object

Post-condition: Comment text appears on the right side bar

### Remove a comment on image
Actor: User

Pre-condition: User has joined or created a whiteboard and opened a comment

Steps:
1. User clicks on the resolve button next to the comment

Post-condition: comment side bar closes and the comment icon disappears from the canvas

## Export board feature

### Export canvas as JPEG or PNG
Actor: User

Pre-condition: User has joined or created a whiteboard

Steps:
1. User selects export tool by clicking the export button on the toolbar.

Post-condition: File download is initiated and user can choose the location to save the file.

## Other *nice to have* features
> These features are not included in the scope of the course, but would be nice additions

### Laser pointer

### Giving a title to whiteboard

### Retain undo history when they rejoin/reopen the session
