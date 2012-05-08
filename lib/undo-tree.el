;;; undo-tree.el --- Treat undo history as a tree

;; Copyright (C) 2009-2012  Free Software Foundation, Inc

;; Author: Toby Cubitt <toby-undo-tree@dr-qubit.org>
;; Version: 0.5.1
;; Keywords: convenience, files, undo, redo, history, tree
;; URL: http://www.dr-qubit.org/emacs.php
;; Repository: http://www.dr-qubit.org/git/undo-tree.git

;; This file is part of Emacs.
;;
;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Emacs has a powerful undo system. Unlike the standard undo/redo system in
;; most software, it allows you to recover *any* past state of a buffer
;; (whereas the standard undo/redo system can lose past states as soon as you
;; redo). However, this power comes at a price: many people find Emacs' undo
;; system confusing and difficult to use, spawning a number of packages that
;; replace it with the less powerful but more intuitive undo/redo system.
;;
;; Both the loss of data with standard undo/redo, and the confusion of Emacs'
;; undo, stem from trying to treat undo history as a linear sequence of
;; changes. It's not. The `undo-tree-mode' provided by this package replaces
;; Emacs' undo system with a system that treats undo history as what it is: a
;; branching tree of changes. This simple idea allows the more intuitive
;; behaviour of the standard undo/redo system to be combined with the power of
;; never losing any history. An added side bonus is that undo history can in
;; some cases be stored more efficiently, allowing more changes to accumulate
;; before Emacs starts discarding history.
;;
;; The only downside to this more advanced yet simpler undo system is that it
;; was inspired by Vim. But, after all, most successful religions steal the
;; best ideas from their competitors!
;;
;;
;; Installation
;; ============
;;
;; This package has only been tested with Emacs versions 22, 23 and CVS. It
;; will not work without modifications in earlier versions of Emacs.
;;
;; To install `undo-tree-mode', make sure this file is saved in a directory in
;; your `load-path', and add the line:
;;
;;   (require 'undo-tree)
;;
;; to your .emacs file. Byte-compiling undo-tree.el is recommended (e.g. using
;; "M-x byte-compile-file" from within emacs).
;;
;; If you want to replace the standard Emacs' undo system with the
;; `undo-tree-mode' system in all buffers, you can enable it globally by
;; adding:
;;
;;   (global-undo-tree-mode)
;;
;; to your .emacs file.
;;
;;
;; Quick-Start
;; ===========
;;
;; If you're the kind of person who likes to jump in the car and drive,
;; without bothering to first figure out whether the button on the left dips
;; the headlights or operates the ejector seat (after all, you'll soon figure
;; it out when you push it), then here's the minimum you need to know:
;;
;; `undo-tree-mode' and `global-undo-tree-mode'
;;   Enable undo-tree mode (either in the current buffer or globally).
;;
;; C-_  C-/  (`undo-tree-undo')
;;   Undo changes.
;;
;; M-_  C-?  (`undo-tree-redo')
;;   Redo changes.
;;
;; `undo-tree-switch-branch'
;;   Switch undo-tree branch.
;;   (What does this mean? Better press the button and see!)
;;
;; C-x u  (`undo-tree-visualize')
;;   Visualize the undo tree.
;;   (Better try pressing this button too!)
;;
;; C-x r u  (`undo-tree-save-state-to-register')
;;   Save current buffer state to register.
;;
;; C-x r U  (`undo-tree-restore-state-from-register')
;;   Restore buffer state from register.
;;
;;
;;
;; In the undo-tree visualizer:
;;
;; <up>  p  C-p  (`undo-tree-visualize-undo')
;;   Undo changes.
;;
;; <down>  n  C-n  (`undo-tree-visualize-redo')
;;   Redo changes.
;;
;; <left>  b  C-b  (`undo-tree-visualize-switch-branch-left')
;;   Switch to previous undo-tree branch.
;;
;; <right>  f  C-f  (`undo-tree-visualize-switch-branch-right')
;;   Switch to next undo-tree branch.
;;
;; <mouse-1>  (`undo-tree-visualizer-mouse-set')
;;   Set state to node at mouse click.
;;
;; t  (`undo-tree-visualizer-toggle-timestamps')
;;   Toggle display of time-stamps.
;;
;; d  (`undo-tree-visualizer-toggle-diff')
;;   Toggle diff display.
;;
;; s  (`undo-tree-visualizer-selection-mode')
;;   Toggle keyboard selection mode.
;;
;; q  (`undo-tree-visualizer-quit')
;;   Quit undo-tree-visualizer.
;;
;; C-q  (`undo-tree-visualizer-abort')
;;   Abort undo-tree-visualizer.
;;
;; ,  <
;;   Scroll left.
;;
;; .  >
;;   Scroll right.
;;
;; <pgup>  M-v
;;   Scroll up.
;;
;; <pgdown>  C-v
;;   Scroll down.
;;
;;
;;
;; In visualizer selection mode:
;;
;; <up>  p  C-p  (`undo-tree-visualizer-select-previous')
;;   Select previous node.
;;
;; <down>  n  C-n  (`undo-tree-visualizer-select-next')
;;   Select next node.
;;
;; <left>  b  C-b  (`undo-tree-visualizer-select-left')
;;   Select left sibling node.
;;
;; <right>  f  C-f  (`undo-tree-visualizer-select-right')
;;   Select right sibling node.
;;
;; <pgup>  M-v
;;   Select node 10 above.
;;
;; <pgdown>  C-v
;;   Select node 10 below.
;;
;; <enter>  (`undo-tree-visualizer-set')
;;   Set state to selected node and exit selection mode.
;;
;; s  (`undo-tree-visualizer-mode')
;;   Exit selection mode.
;;
;; t  (`undo-tree-visualizer-toggle-timestamps')
;;   Toggle display of time-stamps.
;;
;; d  (`undo-tree-visualizer-toggle-diff')
;;   Toggle diff display.
;;
;; q  (`undo-tree-visualizer-quit')
;;   Quit undo-tree-visualizer.
;;
;; C-q  (`undo-tree-visualizer-abort')
;;   Abort undo-tree-visualizer.
;;
;; ,  <
;;   Scroll left.
;;
;; .  >
;;   Scroll right.
;;
;;
;;
;;
;; Undo Systems
;; ============
;;
;; To understand the different undo systems, it's easiest to consider an
;; example. Imagine you make a few edits in a buffer. As you edit, you
;; accumulate a history of changes, which we might visualize as a string of
;; past buffer states, growing downwards:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o  (first edit)
;;                                |
;;                                |
;;                                o  (second edit)
;;                                |
;;                                |
;;                                x  (current buffer state)
;;
;;
;; Now imagine that you undo the last two changes. We can visualize this as
;; rewinding the current state back two steps:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                x  (current buffer state)
;;                                |
;;                                |
;;                                o
;;                                |
;;                                |
;;                                o
;;
;;
;; However, this isn't a good representation of what Emacs' undo system
;; does. Instead, it treats the undos as *new* changes to the buffer, and adds
;; them to the history:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o  (first edit)
;;                                |
;;                                |
;;                                o  (second edit)
;;                                |
;;                                |
;;                                x  (buffer state before undo)
;;                                |
;;                                |
;;                                o  (first undo)
;;                                |
;;                                |
;;                                x  (second undo)
;;
;;
;; Actually, since the buffer returns to a previous state after an undo,
;; perhaps a better way to visualize it is to imagine the string of changes
;; turning back on itself:
;;
;;        (initial buffer state)  o
;;                                |
;;                                |
;;                  (first edit)  o  x  (second undo)
;;                                |  |
;;                                |  |
;;                 (second edit)  o  o  (first undo)
;;                                | /
;;                                |/
;;                                o  (buffer state before undo)
;;
;; Treating undos as new changes might seem a strange thing to do. But the
;; advantage becomes clear as soon as we imagine what happens when you edit
;; the buffer again. Since you've undone a couple of changes, new edits will
;; branch off from the buffer state that you've rewound to. Conceptually, it
;; looks like this:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o
;;                                |\
;;                                | \
;;                                o  x  (new edit)
;;                                |
;;                                |
;;                                o
;;
;; The standard undo/redo system only lets you go backwards and forwards
;; linearly. So as soon as you make that new edit, it discards the old
;; branch. Emacs' undo just keeps adding changes to the end of the string. So
;; the undo history in the two systems now looks like this:
;;
;;            Undo/Redo:                      Emacs' undo
;;
;;               o                                o
;;               |                                |
;;               |                                |
;;               o                                o  o
;;               .\                               |  |\
;;               . \                              |  | \
;;               .  x  (new edit)                 o  o  |
;;   (discarded  .                                | /   |
;;     branch)   .                                |/    |
;;               .                                o     |
;;                                                      |
;;                                                      |
;;                                                      x  (new edit)
;;
;; Now, what if you change your mind about those undos, and decide you did
;; like those other changes you'd made after all? With the standard undo/redo
;; system, you're lost. There's no way to recover them, because that branch
;; was discarded when you made the new edit.
;;
;; However, in Emacs' undo system, those old buffer states are still there in
;; the undo history. You just have to rewind back through the new edit, and
;; back through the changes made by the undos, until you reach them. Of
;; course, since Emacs treats undos (even undos of undos!) as new changes,
;; you're really weaving backwards and forwards through the history, all the
;; time adding new changes to the end of the string as you go:
;;
;;                       o
;;                       |
;;                       |
;;                       o  o     o  (undo new edit)
;;                       |  |\    |\
;;                       |  | \   | \
;;                       o  o  |  |  o  (undo the undo)
;;                       | /   |  |  |
;;                       |/    |  |  |
;;      (trying to get   o     |  |  x  (undo the undo)
;;       to this state)        | /
;;                             |/
;;                             o
;;
;; So far, this is still reasonably intuitive to use. It doesn't behave so
;; differently to standard undo/redo, except that by going back far enough you
;; can access changes that would be lost in standard undo/redo.
;;
;; However, imagine that after undoing as just described, you decide you
;; actually want to rewind right back to the initial state. If you're lucky,
;; and haven't invoked any command since the last undo, you can just keep on
;; undoing until you get back to the start:
;;
;;      (trying to get   o              x  (got there!)
;;       to this state)  |              |
;;                       |              |
;;                       o  o     o     o  (keep undoing)
;;                       |  |\    |\    |
;;                       |  | \   | \   |
;;                       o  o  |  |  o  o  (keep undoing)
;;                       | /   |  |  | /
;;                       |/    |  |  |/
;;      (already undid   o     |  |  o  (got this far)
;;       to this state)        | /
;;                             |/
;;                             o
;;
;; But if you're unlucky, and you happen to have moved the point (say) after
;; getting to the state labelled "got this far", then you've "broken the undo
;; chain". Hold on to something solid, because things are about to get
;; hairy. If you try to undo now, Emacs thinks you're trying to undo the
;; undos! So to get back to the initial state you now have to rewind through
;; *all* the changes, including the undos you just did:
;;
;;      (trying to get   o                          x  (finally got there!)
;;       to this state)  |                          |
;;                       |                          |
;;                       o  o     o     o     o     o
;;                       |  |\    |\    |\    |\    |
;;                       |  | \   | \   | \   | \   |
;;                       o  o  |  |  o  o  o  |  o  o
;;                       | /   |  |  | /   |  |  | /
;;                       |/    |  |  |/    |  |  |/
;;      (already undid   o     |  |  o<.   |  |  o
;;       to this state)        | /     :   | /
;;                             |/      :   |/
;;                             o       :   o
;;                                     :
;;                             (got this far, but
;;                              broke the undo chain)
;;
;; Confused?
;;
;; In practice you can just hold down the undo key until you reach the buffer
;; state that you want. But whatever you do, don't move around in the buffer
;; to *check* that you've got back to where you want! Because you'll break the
;; undo chain, and then you'll have to traverse the entire string of undos
;; again, just to get back to the point at which you broke the
;; chain. Undo-in-region and commands such as `undo-only' help to make using
;; Emacs' undo a little easier, but nonetheless it remains confusing for many
;; people.
;;
;;
;; So what does `undo-tree-mode' do? Remember the diagram we drew to represent
;; the history we've been discussing (make a few edits, undo a couple of them,
;; and edit again)? The diagram that conceptually represented our undo
;; history, before we started discussing specific undo systems? It looked like
;; this:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o
;;                                |\
;;                                | \
;;                                o  x  (current state)
;;                                |
;;                                |
;;                                o
;;
;; Well, that's *exactly* what the undo history looks like to
;; `undo-tree-mode'.  It doesn't discard the old branch (as standard undo/redo
;; does), nor does it treat undos as new changes to be added to the end of a
;; linear string of buffer states (as Emacs' undo does). It just keeps track
;; of the tree of branching changes that make up the entire undo history.
;;
;; If you undo from this point, you'll rewind back up the tree to the previous
;; state:
;;
;;                                o
;;                                |
;;                                |
;;                                x  (undo)
;;                                |\
;;                                | \
;;                                o  o
;;                                |
;;                                |
;;                                o
;;
;; If you were to undo again, you'd rewind back to the initial state. If on
;; the other hand you redo the change, you'll end up back at the bottom of the
;; most recent branch:
;;
;;                                o  (undo takes you here)
;;                                |
;;                                |
;;                                o  (start here)
;;                                |\
;;                                | \
;;                                o  x  (redo takes you here)
;;                                |
;;                                |
;;                                o
;;
;; So far, this is just like the standard undo/redo system. But what if you
;; want to return to a buffer state located on a previous branch of the
;; history? Since `undo-tree-mode' keeps the entire history, you simply need
;; to tell it to switch to a different branch, and then redo the changes you
;; want:
;;
;;                                o
;;                                |
;;                                |
;;                                o  (start here, but switch
;;                                |\  to the other branch)
;;                                | \
;;                        (redo)  o  o
;;                                |
;;                                |
;;                        (redo)  x
;;
;; Now you're on the other branch, if you undo and redo changes you'll stay on
;; that branch, moving up and down through the buffer states located on that
;; branch. Until you decide to switch branches again, of course.
;;
;; Real undo trees might have multiple branches and sub-branches:
;;
;;                                o
;;                            ____|______
;;                           /           \
;;                          o             o
;;                      ____|__         __|
;;                     /    |  \       /   \
;;                    o     o   o     o     x
;;                    |               |
;;                   / \             / \
;;                  o   o           o   o
;;
;; Trying to imagine what Emacs' undo would do as you move about such a tree
;; will likely frazzle your brain circuits! But in `undo-tree-mode', you're
;; just moving around this undo history tree. Most of the time, you'll
;; probably only need to stay on the most recent branch, in which case it
;; behaves like standard undo/redo, and is just as simple to understand. But
;; if you ever need to recover a buffer state on a different branch, the
;; possibility of switching between branches and accessing the full undo
;; history is still there.
;;
;;
;;
;; The Undo-Tree Visualizer
;; ========================
;;
;; Actually, it gets better. You don't have to imagine all these tree
;; diagrams, because `undo-tree-mode' includes an undo-tree visualizer which
;; draws them for you! In fact, it draws even better diagrams: it highlights
;; the node representing the current buffer state, it highlights the current
;; branch, and you can toggle the display of time-stamps (by hitting "t") and
;; a diff of the undo changes (by hitting "d"). (There's one other tiny
;; difference: the visualizer puts the most recent branch on the left rather
;; than the right.)
;;
;; Bring up the undo tree visualizer whenever you want by hitting "C-x u".
;;
;; In the visualizer, the usual keys for moving up and down a buffer instead
;; move up and down the undo history tree (e.g. the up and down arrow keys, or
;; "C-n" and "C-p"). The state of the "parent" buffer (the buffer whose undo
;; history you are visualizing) is updated as you move around the undo tree in
;; the visualizer. If you reach a branch point in the visualizer, the usual
;; keys for moving forward and backward in a buffer instead switch branch
;; (e.g. the left and right arrow keys, or "C-f" and "C-b").
;;
;; Clicking with the mouse on any node in the visualizer will take you
;; directly to that node, resetting the state of the parent buffer to the
;; state represented by that node.
;;
;; You can also select nodes directly using the keyboard, by hitting "s" to
;; toggle selection mode. The usual motion keys now allow you to move around
;; the tree without changing the parent buffer. Hitting <enter> will reset the
;; state of the parent buffer to the state represented by the currently
;; selected node.
;;
;; It can be useful to see how long ago the parent buffer was in the state
;; represented by a particular node in the visualizer. Hitting "t" in the
;; visualizer toggles the display of time-stamps for all the nodes. (Note
;; that, because of the way `undo-tree-mode' works, these time-stamps may be
;; somewhat later than the true times, especially if it's been a long time
;; since you last undid any changes.)
;;
;; To get some idea of what changes are represented by a given node in the
;; tree, it can be useful to see a diff of the changes. Hit "d" in the
;; visualizer to toggle a diff display. This normally displays a diff between
;; the current state and the previous one, i.e. it shows you the changes that
;; will be applied if you undo (move up the tree). However, the diff display
;; really comes into its own in the visualizer's selection mode (see above),
;; where it instead shows a diff between the current state and the currently
;; selected state, i.e. it shows you the changes that will be applied if you
;; reset to the selected state.
;;
;; (Note that the diff is generated by the Emacs `diff' command, and is
;; displayed using `diff-mode'. See the corresponding customization groups if
;; you want to customize the diff display.)
;;
;; Finally, hitting "q" will quit the visualizer, leaving the parent buffer in
;; whatever state you ended at. Hitting "C-q" will abort the visualizer,
;; returning the parent buffer to whatever state it was originally in when the
;; visualizer was .
;;
;;
;;
;; Undo-in-Region
;; ==============
;;
;; Emacs allows a very useful and powerful method of undoing only selected
;; changes: when a region is active, only changes that affect the text within
;; that region will be undone. With the standard Emacs undo system, changes
;; produced by undoing-in-region naturally get added onto the end of the
;; linear undo history:
;;
;;                       o
;;                       |
;;                       |  x  (second undo-in-region)
;;                       o  |
;;                       |  |
;;                       |  o  (first undo-in-region)
;;                       o  |
;;                       | /
;;                       |/
;;                       o
;;
;; You can of course redo these undos-in-region as usual, by undoing the
;; undos:
;;
;;                       o
;;                       |
;;                       |  o_
;;                       o  | \
;;                       |  |  |
;;                       |  o  o  (undo the undo-in-region)
;;                       o  |  |
;;                       | /   |
;;                       |/    |
;;                       o     x  (undo the undo-in-region)
;;
;;
;; In `undo-tree-mode', undo-in-region works similarly: when there's an active
;; region, undoing only undoes changes that affect that region. However, the
;; way these undos-in-region are recorded in the undo history is quite
;; different. In `undo-tree-mode', undo-in-region creates a new branch in the
;; undo history. The new branch consists of an undo step that undoes some of
;; the changes that affect the current region, and another step that undoes
;; the remaining changes needed to rejoin the previous undo history.
;;
;;      Previous undo history                Undo-in-region
;;
;;               o                                o
;;               |                                |
;;               |                                |
;;               o                                o
;;               |                                |\
;;               |                                | \
;;               o                                o  x  (undo-in-region)
;;               |                                |  |
;;               |                                |  |
;;               x                                o  o
;;
;; As long as you don't change the active region after undoing-in-region,
;; continuing to undo-in-region extends the new branch, pulling more changes
;; that affect the current region into an undo step immediately above your
;; current location in the undo tree, and pushing the point at which the new
;; branch is attached further up the tree:
;;
;;      First undo-in-region                 Second undo-in-region
;;
;;               o                                o
;;               |                                |\
;;               |                                | \
;;               o                                o  x  (undo-in-region)
;;               |\                               |  |
;;               | \                              |  |
;;               o  x                             o  o
;;               |  |                             |  |
;;               |  |                             |  |
;;               o  o                             o  o
;;
;; Redoing takes you back down the undo tree, as usual (as long as you haven't
;; changed the active region after undoing-in-region, it doesn't matter if it
;; is still active):
;;
;;                       o
;;			 |\
;;			 | \
;;			 o  o
;;			 |  |
;;			 |  |
;;			 o  o  (redo)
;;			 |  |
;;			 |  |
;;			 o  x  (redo)
;;
;;
;; What about redo-in-region? Obviously, this only makes sense if you have
;; already undone some changes, so that there are some changes to redo!
;; Redoing-in-region splits off a new branch of the undo history below your
;; current location in the undo tree. This time, the new branch consists of a
;; redo step that redoes some of the redo changes that affect the current
;; region, followed by all the remaining redo changes.
;;
;;      Previous undo history                Redo-in-region
;;
;;               o                                o
;;               |                                |
;;               |                                |
;;               x                                o
;;               |                                |\
;;               |                                | \
;;               o                                o  x  (redo-in-region)
;;               |                                |  |
;;               |                                |  |
;;               o                                o  o
;;
;; As long as you don't change the active region after redoing-in-region,
;; continuing to redo-in-region extends the new branch, pulling more redo
;; changes into a redo step immediately below your current location in the
;; undo tree.
;;
;;      First redo-in-region                 Second redo-in-region
;;
;;          o                                     o
;;          |                                     |
;;          |                                     |
;;          o                                     o
;;          |\                                    |\
;;          | \                                   | \
;;          o  x  (redo-in-region)                o  o
;;          |  |                                  |  |
;;          |  |                                  |  |
;;          o  o                                  o  x  (redo-in-region)
;;                                                   |
;;                                                   |
;;                                                   o
;;
;; Note that undo-in-region and redo-in-region only ever add new changes to
;; the undo tree, they *never* modify existing undo history. So you can always
;; return to previous buffer states by switching to a previous branch of the
;; tree.



;;; Change Log:
;;
;; Version 0.5.1
;; * remove now unnecessary compatibility hack for `called-interactively-p'
;;
;; Version 0.5
;; * implemented diff display in visualizer, toggled on and off using
;;   `undo-tree-visualizer-toggle-diff'
;; * added `undo-tree-visualizer-diff' customization option, to display diff
;;   by default
;; * added `called-interactively-p', `registerv-make', `registerv-data',
;;   `diff-no-select' and `diff-file-local-copy' compatibility hacks for
;;   older Emacsen
;; * split out core of `undo-tree-undo' and `undo-tree-redo' into internal
;;   `undo-tree-undo-1' and `undo-tree-redo-1' functions, which now take an
;;   additional optional argument to preserve timestamps
;; * preserve timestamps when generating diff for visualizer diff view
;; * fixed bug in `undo-tree-visualizer-select-left' and
;;   `undo-tree-visualizer-select-right' when using selection mode whilst
;;   timestamps are displayed
;; * fixed bug in `undo-tree-draw-node' caused by new registerv structure,
;;   which prevented registers from being displayed in visualizer
;; * added `undo-tree-visualizer-relative-timestamps' option to make
;;   visualizer  display timestamps relative to current time
;; * use a function `undo-tree-make-history-save-file-name' function to
;;   generate history save filename, allowing save file to be customized by
;;   overriding this function
;; * clear visualizer data / kill visualizer in `undo-tree-save-history'
;;   before saving history to file, otherwise markers in visualizer meta-data
;;   cause read errors in `undo-tree-load-history'
;; * make `undo-tree-visualizer-timestamps' into defcustom, to allow
;;   timestamps to be displayed by default
;; * use `undo-tree-visualizer-selected-node' to store currently selected node
;;   in visualizer selection mode, instead of relying on point location, to
;;   avoid errors if point was moved manually
;; * added `undo-tree-visualizer-abort' command to quit visualizer and return
;;   to original state, stored in `undo-tree-visualizer-initial-node'
;;
;; Version 0.4
;; * implemented persistent history storage: `undo-tree-save-history' and
;;   `undo-tree-load-history' save and restore an undo tree to file, enabling
;;   `undo-tree-auto-save-history' causes history to be saved and restored
;;   automatically when saving or loading files
;; * renamed internal `make-undo-tree-<struct>' functions to
;;   `undo-tree-make-<struct>' to avoid polluting name-space
;; * create proper registerv structure using `registerv-make' when storing
;;   undo state in registers in `undo-tree-save-state-to-register' (and
;;   `undo-tree-restore-state-from-register')
;; * suppress branch point messages when undo/redoing from `undo-tree-set'
;; * make various interactive commands signal an error if buffer is read-only
;; * let-bind `inhibit-read-only' instead of setting and restoring
;;   `buffer-read-only'
;; * use non-nil `undo-tree-inhibit-kill-visualizer' instead of
;;   `undo-in-progress' to inhibit `undo-tree-kill-visualizer', so that
;;   undoing and redoing in parent buffer also kill visualizer
;;
;; Version 0.3.5
;; * improved `undo-tree-switch-branch': display current branch number in
;;   prompt, switch to other branch without prompting when there are only two,
;;   and display message indicating new branch number after switching
;;
;; Version 0.3.4
;; * set `permanent-local' property on `buffer-undo-tree', to prevent history
;;   being discarded when switching major-mode
;; * added `undo-tree-enable-undo-in-region' customization option to allow
;;   undo-in-region to be disabled.
;; * fixed bug in `undo-list-pop-changeset' which, through a subtle chain of
;;   consequences, occasionally caused undo-tree-mode to lose large amounts of
;;   undo history (thanks to Magnar Sveen for his sterling efforts in helping
;;   track this down!)
;;
;; Version 0.3.3;
;; * added `term-mode' to `undo-tree-incompatible-major-modes'
;;
;; Version 0.3.2
;; * added additional check in `undo-list-GCd-marker-elt-p' to guard against
;;   undo elements being mis-identified as marker elements
;; * fixed bug in `undo-list-transfer-to-tree'
;;
;; Version 0.3.1
;; * use `get-buffer-create' when creating the visualizer buffer in
;;   `undo-tree-visualize', to fix bug caused by `global-undo-tree-mode' being
;;   enabled in the visualizer when `default-major-mode' is set to something
;;   other than `fundamental-mode' (thanks to Michael Heerdegen for suggesting
;;   this fix)
;; * modified `turn-on-undo-tree-mode' to avoid turning on `undo-tree-mode' if
;;   the buffer's `major-mode' implements its own undo system, by checking
;;   whether `undo' is remapped, the default "C-/" or "C-_" bindings have been
;;   overridden,  or the `major-mode' is listed in
;;   `undo-tree-incompatible-major-modes'
;; * discard position entries from `buffer-undo-list' changesets created by
;;   undoing or redoing, to ensure point is always moved to where the change
;;   is (standard Emacs `undo' also does this)
;; * fixed `undo-tree-draw-node' to use correct faces and indicate registers
;;   when displaying timestamps in visualizer
;;
;; Version 0.3
;; * implemented undo-in-region
;; * fixed bugs in `undo-list-transfer-to-tree' and
;;   `undo-list-rebuild-from-tree' which caused errors when undo history was
;;   empty or disabled
;; * defun `region-active-p' if not already defined, for compatibility with
;;   older Emacsen
;;
;; Version 0.2.1
;; * modified `undo-tree-node' defstruct and macros to allow arbitrary
;;   meta-data to be stored in a plist associated with a node, and
;;   reimplemented storage of visualizer data on top of this
;; * display registers storing undo-tree state in visualizer
;; * implemented keyboard selection in visualizer
;; * rebuild `buffer-undo-list' from tree when disabling `undo-tree-mode'
;;
;; Version 0.2
;; * added support for marker undo entries
;;
;; Version 0.1.7
;; * pass null argument to `kill-buffer' call in `undo-tree-visualizer-quit',
;;   since the argument's not optional in earlier Emacs versions
;; * added match for "No further redo information" to
;;   `debug-ignored-errors' to prevent debugger being called on this error
;; * made `undo-tree-visualizer-quit' select the window displaying the
;;   visualizer's parent buffer, or switch to the parent buffer if no window
;;   is displaying it
;; * fixed bug in `undo-tree-switch-branch'
;; * general code tidying and reorganisation
;; * fixed bugs in history-discarding logic
;; * fixed bug in `undo-tree-insert' triggered by `undo-tree-visualizer-set'
;;   by ensuring mark is deactivated
;;
;; Version 0.1.6
;; * added `undo-tree-mode-lighter' customization option to allow the
;;   mode-line lighter to be changed
;; * bug-fix in `undo-tree-discard-node'
;; * added `undo-tree-save-state-to-register' and
;;   `undo-tree-restore-state-from-register' commands and keybindings for
;;   saving/restoring undo-tree states using registers
;;
;; Version 0.1.5
;; * modified `undo-tree-visualize' to mark the visualizer window as
;;   soft-dedicated, and changed `undo-tree-visualizer-quit' to use
;;   `kill-buffer', so that the visualizer window is deleted along with its
;;   buffer if the visualizer buffer was displayed in a new window, but not if
;;   it was displayed in an existing window.
;;
;; Version 0.1.4
;; * modified `undo-tree-undo' and `undo-tree-redo' to always replace
;;   redo/undo entries with new ones generated by `primitive-undo', as the new
;;   changesets will restore the point more reliably
;;
;; Version 0.1.3
;; * fixed `undo-tree-visualizer-quit' to remove `after-change-functions'
;;   hook there, rather than in `undo-tree-kill-visualizer'
;;
;; Version 0.1.2
;; * fixed keybindings
;; * renamed `undo-tree-visualizer-switch-previous-branch' and
;;   `undo-tree-visualizer-switch-next-branch' to
;;   `undo-tree-visualizer-switch-branch-left' and
;;   `undo-tree-visualizer-switch-branch-right'
;;
;; Version 0.1.1
;; * prevented `undo-tree-kill-visualizer' from killing visualizer when
;;   undoing/redoing from the visualizer, which completely broke the
;;   visualizer!
;; * changed one redo binding, so that at least one set of undo/redo bindings
;;   works in a terminal
;; * bound vertical scrolling commands in `undo-tree-visualizer-map', in case
;;   they aren't bound globally
;; * added missing :group argument to `defface's
;;
;; Version 0.1
;; * initial release



;;; Code:

(eval-when-compile (require 'cl))
(require 'diff)



;;; =====================================================================
;;;              Compatibility hacks for older Emacsen

;; `characterp' isn't defined in Emacs versions < 23
(unless (fboundp 'characterp)
  (defalias 'characterp 'char-valid-p))

;; `region-active-p' isn't defined in Emacs versions < 23
(unless (fboundp 'region-active-p)
  (defun region-active-p () (and transient-mark-mode mark-active)))


;; `registerv' defstruct isn't defined in Emacs versions < 24
(unless (fboundp 'registerv-make)
  (defmacro registerv-make (data &rest dummy) data))

(unless (fboundp 'registerv-data)
  (defmacro registerv-data (data) data))


;; `diff-no-select' and `diff-file-local-copy' aren't defined in Emacs
;; versions < 24 (copied and adapted from Emacs 24)
(unless (fboundp 'diff-no-select)
  (defun diff-no-select (old new &optional switches no-async buf)
    ;; Noninteractive helper for creating and reverting diff buffers
    (unless (bufferp new) (setq new (expand-file-name new)))
    (unless (bufferp old) (setq old (expand-file-name old)))
    (or switches (setq switches diff-switches)) ; If not specified, use default.
    (unless (listp switches) (setq switches (list switches)))
    (or buf (setq buf (get-buffer-create "*Diff*")))
    (let* ((old-alt (diff-file-local-copy old))
	   (new-alt (diff-file-local-copy new))
	   (command
	    (mapconcat 'identity
		       `(,diff-command
			 ;; Use explicitly specified switches
			 ,@switches
			 ,@(mapcar #'shell-quote-argument
				   (nconc
				    (when (or old-alt new-alt)
				      (list "-L" (if (stringp old)
						     old (prin1-to-string old))
					    "-L" (if (stringp new)
						     new (prin1-to-string new))))
				    (list (or old-alt old)
					  (or new-alt new)))))
		       " "))
	   (thisdir default-directory))
      (with-current-buffer buf
	(setq buffer-read-only t)
	(buffer-disable-undo (current-buffer))
	(let ((inhibit-read-only t))
	  (erase-buffer))
	(buffer-enable-undo (current-buffer))
	(diff-mode)
	(set (make-local-variable 'revert-buffer-function)
	     (lambda (_ignore-auto _noconfirm)
	       (diff-no-select old new switches no-async (current-buffer))))
	(setq default-directory thisdir)
	(let ((inhibit-read-only t))
	  (insert command "\n"))
	(if (and (not no-async) (fboundp 'start-process))
	    (let ((proc (start-process "Diff" buf shell-file-name
				       shell-command-switch command)))
	      (set-process-filter proc 'diff-process-filter)
	      (set-process-sentinel
	       proc (lambda (proc _msg)
		      (with-current-buffer (process-buffer proc)
			(diff-sentinel (process-exit-status proc))
			(if old-alt (delete-file old-alt))
			(if new-alt (delete-file new-alt))))))
	  ;; Async processes aren't available.
	  (let ((inhibit-read-only t))
	    (diff-sentinel
	     (call-process shell-file-name nil buf nil
			   shell-command-switch command))
	    (if old-alt (delete-file old-alt))
	    (if new-alt (delete-file new-alt)))))
      buf)))

(unless (fboundp 'diff-file-local-copy)
  (defun diff-file-local-copy (file-or-buf)
    (if (bufferp file-or-buf)
	(with-current-buffer file-or-buf
	  (let ((tempfile (make-temp-file "buffer-content-")))
	    (write-region nil nil tempfile nil 'nomessage)
	    tempfile))
      (file-local-copy file-or-buf))))




;;; =====================================================================
;;;              Global variables and customization options

(defvar buffer-undo-tree nil
  "Tree of undo entries in current buffer.")
(make-variable-buffer-local 'buffer-undo-tree)
(put 'buffer-undo-tree 'permanent-local t)


(defgroup undo-tree nil
  "Tree undo/redo."
  :group 'undo)

(defcustom undo-tree-mode-lighter " Undo-Tree"
  "Lighter displayed in mode line
when `undo-tree-mode' is enabled."
  :group 'undo-tree
  :type 'string)


(defcustom undo-tree-auto-save-history nil
  "When non-nil, `undo-tree-mode' will save undo history to file
when a buffer is saved to file.

It will automatically load undo history when a buffer is loaded
from file, if an undo save file exists.

Undo-tree history is saved to a file called
\".<buffer-file-name>.~undo-tree\" in the same directory as the
file itself.

WARNING! `undo-tree-auto-save-history' will not work properly in
Emacs versions prior to 24.1.50.1, so it cannot be enabled via
the customization interface in versions earlier than that one. To
ignore this warning and enable it regardless, set
`undo-tree-auto-save-history' to a non-nil value outside of
customize."
  :group 'undo-tree
  :type (if (version-list-< (version-to-list emacs-version) '(24 1 50 1))
	    '(choice (const :tag "<disabled>" nil))
	  'boolean))


(defcustom undo-tree-visualizer-relative-timestamps t
  "When non-nil, display times relative to current time
when displaying time stamps in visualizer.

Otherwise, display absolute times."
  :group 'undo-tree
  :type 'boolean)


(defcustom undo-tree-visualizer-timestamps nil
  "When non-nil, display time-stamps by default
in undo-tree visualizer.

\\<undo-tree-visualizer-map>You can always toggle time-stamps on and off \
using \\[undo-tree-visualizer-toggle-timestamps], regardless of the
setting of this variable."
  :group 'undo-tree
  :type 'boolean)
(make-variable-buffer-local 'undo-tree-visualizer-timestamps)


(defcustom undo-tree-visualizer-diff nil
  "When non-nil, display diff by default in undo-tree visualizer.

\\<undo-tree-visualizer-map>You can always toggle the diff display \
using \\[undo-tree-visualizer-toggle-diff], regardless of the
setting of this variable."
  :group 'undo-tree
  :type 'boolean)
(make-variable-buffer-local 'undo-tree-visualizer-diff)


(defcustom undo-tree-incompatible-major-modes '(term-mode)
  "List of major-modes in which `undo-tree-mode' should not be enabled.
\(See `turn-on-undo-tree-mode'.\)"
  :group 'undo-tree
  :type '(repeat symbol))


(defcustom undo-tree-enable-undo-in-region t
  "When non-nil, enable undo-in-region.

When undo-in-region is enabled, undoing or redoing when the
region is active (in `transient-mark-mode') or with a prefix
argument (not in `transient-mark-mode') only undoes changes
within the current region."
  :group 'undo-tree
  :type 'boolean)


(defface undo-tree-visualizer-default-face
  '((((class color)) :foreground "gray"))
  "Face used to draw undo-tree in visualizer."
  :group 'undo-tree)

(defface undo-tree-visualizer-current-face
  '((((class color)) :foreground "red"))
  "Face used to highlight current undo-tree node in visualizer."
  :group 'undo-tree)

(defface undo-tree-visualizer-active-branch-face
  '((((class color) (background dark))
     (:foreground "white" :weight bold))
    (((class color) (background light))
     (:foreground "black" :weight bold)))
  "Face used to highlight active undo-tree branch in visualizer."
  :group 'undo-tree)

(defface undo-tree-visualizer-register-face
  '((((class color)) :foreground "yellow"))
  "Face used to highlight undo-tree nodes saved to a register
in visualizer."
  :group 'undo-tree)


(defvar undo-tree-visualizer-parent-buffer nil
  "Parent buffer in visualizer.")
(make-variable-buffer-local 'undo-tree-visualizer-parent-buffer)

;; stores current horizontal spacing needed for drawing undo-tree
(defvar undo-tree-visualizer-spacing nil)
(make-variable-buffer-local 'undo-tree-visualizer-spacing)

;; calculate horizontal spacing required for drawing undo-tree with current
;; settings
(defsubst undo-tree-visualizer-calculate-spacing ()
  (if undo-tree-visualizer-timestamps
      (if undo-tree-visualizer-relative-timestamps 9 13)
    3))

;; holds node that was current when visualizer was invoked
(defvar undo-tree-visualizer-initial-node nil)
(make-variable-buffer-local 'undo-tree-visualizer-initial-node)

;; holds currently selected node in visualizer selection mode
(defvar undo-tree-visualizer-selected-node nil)
(make-variable-buffer-local 'undo-tree-visualizer-selected)

;; dynamically bound to t when undoing from visualizer, to inhibit
;; `undo-tree-kill-visualizer' hook function in parent buffer
(defvar undo-tree-inhibit-kill-visualizer nil)


(defconst undo-tree-visualizer-buffer-name " *undo-tree*")
(defconst undo-tree-diff-buffer-name "*undo-tree Diff*")

;; prevent debugger being called on "No further redo information"
(add-to-list 'debug-ignored-errors "^No further redo information")




;;; =================================================================
;;;                     Setup default keymaps

(defvar undo-tree-map nil
  "Keymap used in undo-tree-mode.")

(unless undo-tree-map
  (let ((map (make-sparse-keymap)))
    ;; remap `undo' and `undo-only' to `undo-tree-undo'
    (define-key map [remap undo] 'undo-tree-undo)
    (define-key map [remap undo-only] 'undo-tree-undo)
    ;; bind standard undo bindings (since these match redo counterparts)
    (define-key map (kbd "C-/") 'undo-tree-undo)
    (define-key map "\C-_" 'undo-tree-undo)
    ;; redo doesn't exist normally, so define our own keybindings
    (define-key map (kbd "C-?") 'undo-tree-redo)
    (define-key map (kbd "M-_") 'undo-tree-redo)
    ;; just in case something has defined `redo'...
    (define-key map [remap redo] 'undo-tree-redo)
    ;; we use "C-x u" for the undo-tree visualizer
    (define-key map (kbd "\C-x u") 'undo-tree-visualize)
    ;; bind register commands
    (define-key map (kbd "C-x r u") 'undo-tree-save-state-to-register)
    (define-key map (kbd "C-x r U") 'undo-tree-restore-state-from-register)
    ;; set keymap
    (setq undo-tree-map map)))


(defvar undo-tree-visualizer-map nil
  "Keymap used in undo-tree visualizer.")

(unless undo-tree-visualizer-map
  (let ((map (make-sparse-keymap)))
    ;; vertical motion keys undo/redo
    (define-key map [remap previous-line] 'undo-tree-visualize-undo)
    (define-key map [remap next-line] 'undo-tree-visualize-redo)
    (define-key map [up] 'undo-tree-visualize-undo)
    (define-key map "p" 'undo-tree-visualize-undo)
    (define-key map "\C-p" 'undo-tree-visualize-undo)
    (define-key map [down] 'undo-tree-visualize-redo)
    (define-key map "n" 'undo-tree-visualize-redo)
    (define-key map "\C-n" 'undo-tree-visualize-redo)
    ;; horizontal motion keys switch branch
    (define-key map [remap forward-char]
      'undo-tree-visualize-switch-branch-right)
    (define-key map [remap backward-char]
      'undo-tree-visualize-switch-branch-left)
    (define-key map [right] 'undo-tree-visualize-switch-branch-right)
    (define-key map "f" 'undo-tree-visualize-switch-branch-right)
    (define-key map "\C-f" 'undo-tree-visualize-switch-branch-right)
    (define-key map [left] 'undo-tree-visualize-switch-branch-left)
    (define-key map "b" 'undo-tree-visualize-switch-branch-left)
    (define-key map "\C-b" 'undo-tree-visualize-switch-branch-left)
    ;; mouse sets buffer state to node at click
    (define-key map [mouse-1] 'undo-tree-visualizer-mouse-set)
    ;; toggle timestamps
    (define-key map "t" 'undo-tree-visualizer-toggle-timestamps)
    ;; toggle diff
    (define-key map "d" 'undo-tree-visualizer-toggle-diff)
    ;; selection mode
    (define-key map "s" 'undo-tree-visualizer-selection-mode)
    ;; horizontal scrolling may be needed if the tree is very wide
    (define-key map "," 'undo-tree-visualizer-scroll-left)
    (define-key map "." 'undo-tree-visualizer-scroll-right)
    (define-key map "<" 'undo-tree-visualizer-scroll-left)
    (define-key map ">" 'undo-tree-visualizer-scroll-right)
    ;; vertical scrolling may be needed if the tree is very tall
    (define-key map [next] 'scroll-up)
    (define-key map [prior] 'scroll-down)
    ;; quit/abort visualizer
    (define-key map "q" 'undo-tree-visualizer-quit)
    (define-key map "\C-q" 'undo-tree-visualizer-abort)
    ;; set keymap
    (setq undo-tree-visualizer-map map)))


(defvar undo-tree-visualizer-selection-map nil
  "Keymap used in undo-tree visualizer selection mode.")

(unless undo-tree-visualizer-selection-map
  (let ((map (make-sparse-keymap)))
    ;; vertical motion keys move up and down tree
    (define-key map [remap previous-line]
      'undo-tree-visualizer-select-previous)
    (define-key map [remap next-line]
      'undo-tree-visualizer-select-next)
    (define-key map [up] 'undo-tree-visualizer-select-previous)
    (define-key map "p" 'undo-tree-visualizer-select-previous)
    (define-key map "\C-p" 'undo-tree-visualizer-select-previous)
    (define-key map [down] 'undo-tree-visualizer-select-next)
    (define-key map "n" 'undo-tree-visualizer-select-next)
    (define-key map "\C-n" 'undo-tree-visualizer-select-next)
    ;; vertical scroll keys move up and down quickly
    (define-key map [next]
      (lambda () (interactive) (undo-tree-visualizer-select-next 10)))
    (define-key map [prior]
      (lambda () (interactive) (undo-tree-visualizer-select-previous 10)))
    ;; horizontal motion keys move to left and right siblings
    (define-key map [remap forward-char] 'undo-tree-visualizer-select-right)
    (define-key map [remap backward-char] 'undo-tree-visualizer-select-left)
    (define-key map [right] 'undo-tree-visualizer-select-right)
    (define-key map "f" 'undo-tree-visualizer-select-right)
    (define-key map "\C-f" 'undo-tree-visualizer-select-right)
    (define-key map [left] 'undo-tree-visualizer-select-left)
    (define-key map "b" 'undo-tree-visualizer-select-left)
    (define-key map "\C-b" 'undo-tree-visualizer-select-left)
    ;; horizontal scroll keys move left or right quickly
    (define-key map ","
      (lambda () (interactive) (undo-tree-visualizer-select-left 10)))
    (define-key map "."
      (lambda () (interactive) (undo-tree-visualizer-select-right 10)))
    (define-key map "<"
      (lambda () (interactive) (undo-tree-visualizer-select-left 10)))
    (define-key map ">"
      (lambda () (interactive) (undo-tree-visualizer-select-right 10)))
    ;; mouse or <enter> sets buffer state to node at point/click
    (define-key map "\r" 'undo-tree-visualizer-set)
    (define-key map [mouse-1] 'undo-tree-visualizer-mouse-set)
    ;; toggle timestamps
    (define-key map "t" 'undo-tree-visualizer-toggle-timestamps)
    ;; toggle diff
    (define-key map "d" 'undo-tree-visualizer-selection-toggle-diff)
    ;; quit visualizer selection mode
    (define-key map "s" 'undo-tree-visualizer-mode)
    ;; quit visualizer
    (define-key map "q" 'undo-tree-visualizer-quit)
    (define-key map "\C-q" 'undo-tree-visualizer-abort)
    ;; set keymap
    (setq undo-tree-visualizer-selection-map map)))




;;; =====================================================================
;;;                     Undo-tree data structure

(defstruct
  (undo-tree
   :named
   (:constructor nil)
   (:constructor make-undo-tree
                 (&aux
                  (root (undo-tree-make-node nil nil))
                  (current root)
                  (size 0)
		  (object-pool (make-hash-table :test 'eq :weakness 'value))))
   ;;(:copier nil)
   )
  root current size object-pool)



(defstruct
  (undo-tree-node
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor undo-tree-make-node
                 (previous undo
		  &optional redo
                  &aux
                  (timestamp (current-time))
                  (branch 0)))
   (:constructor undo-tree-make-node-backwards
                 (next-node undo
		  &optional redo
                  &aux
                  (next (list next-node))
                  (timestamp (current-time))
                  (branch 0)))
   (:copier nil))
  previous next undo redo timestamp branch meta-data)


(defmacro undo-tree-node-p (n)
  (let ((len (length (undo-tree-make-node nil nil))))
    `(and (vectorp ,n) (= (length ,n) ,len))))



(defstruct
  (undo-tree-region-data
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor undo-tree-make-region-data
		 (&optional undo-beginning undo-end
			     redo-beginning redo-end))
   (:constructor undo-tree-make-undo-region-data
		 (undo-beginning undo-end))
   (:constructor undo-tree-make-redo-region-data
		 (redo-beginning redo-end))
   (:copier nil))
  undo-beginning undo-end redo-beginning redo-end)


(defmacro undo-tree-region-data-p (r)
  (let ((len (length (undo-tree-make-region-data))))
    `(and (vectorp ,r) (= (length ,r) ,len))))

(defmacro undo-tree-node-clear-region-data (node)
  `(setf (undo-tree-node-meta-data ,node)
	 (delq nil
	       (delq :region
		     (plist-put (undo-tree-node-meta-data ,node)
				:region nil)))))


(defmacro undo-tree-node-undo-beginning (node)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (when (undo-tree-region-data-p r)
       (undo-tree-region-data-undo-beginning r))))

(defmacro undo-tree-node-undo-end (node)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (when (undo-tree-region-data-p r)
       (undo-tree-region-data-undo-end r))))

(defmacro undo-tree-node-redo-beginning (node)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (when (undo-tree-region-data-p r)
       (undo-tree-region-data-redo-beginning r))))

(defmacro undo-tree-node-redo-end (node)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (when (undo-tree-region-data-p r)
       (undo-tree-region-data-redo-end r))))


(defsetf undo-tree-node-undo-beginning (node) (val)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (unless (undo-tree-region-data-p r)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :region
			(setq r (undo-tree-make-region-data)))))
     (setf (undo-tree-region-data-undo-beginning r) ,val)))

(defsetf undo-tree-node-undo-end (node) (val)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (unless (undo-tree-region-data-p r)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :region
			(setq r (undo-tree-make-region-data)))))
     (setf (undo-tree-region-data-undo-end r) ,val)))

(defsetf undo-tree-node-redo-beginning (node) (val)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (unless (undo-tree-region-data-p r)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :region
			(setq r (undo-tree-make-region-data)))))
     (setf (undo-tree-region-data-redo-beginning r) ,val)))

(defsetf undo-tree-node-redo-end (node) (val)
  `(let ((r (plist-get (undo-tree-node-meta-data ,node) :region)))
     (unless (undo-tree-region-data-p r)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :region
			(setq r (undo-tree-make-region-data)))))
     (setf (undo-tree-region-data-redo-end r) ,val)))



(defstruct
  (undo-tree-visualizer-data
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor undo-tree-make-visualizer-data
		 (&optional lwidth cwidth rwidth marker))
   (:copier nil))
  lwidth cwidth rwidth marker)


(defmacro undo-tree-visualizer-data-p (v)
  (let ((len (length (undo-tree-make-visualizer-data))))
    `(and (vectorp ,v) (= (length ,v) ,len))))

(defmacro undo-tree-node-clear-visualizer-data (node)
  `(setf (undo-tree-node-meta-data ,node)
	 (delq nil
	       (delq :visualizer
		     (plist-put (undo-tree-node-meta-data ,node)
				:visualizer nil)))))


(defmacro undo-tree-node-lwidth (node)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (when (undo-tree-visualizer-data-p v)
       (undo-tree-visualizer-data-lwidth v))))

(defmacro undo-tree-node-cwidth (node)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (when (undo-tree-visualizer-data-p v)
       (undo-tree-visualizer-data-cwidth v))))

(defmacro undo-tree-node-rwidth (node)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (when (undo-tree-visualizer-data-p v)
       (undo-tree-visualizer-data-rwidth v))))

(defmacro undo-tree-node-marker (node)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (when (undo-tree-visualizer-data-p v)
       (undo-tree-visualizer-data-marker v))))


(defsetf undo-tree-node-lwidth (node) (val)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :visualizer
			(setq v (undo-tree-make-visualizer-data)))))
     (setf (undo-tree-visualizer-data-lwidth v) ,val)))

(defsetf undo-tree-node-cwidth (node) (val)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :visualizer
			(setq v (undo-tree-make-visualizer-data)))))
     (setf (undo-tree-visualizer-data-cwidth v) ,val)))

(defsetf undo-tree-node-rwidth (node) (val)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :visualizer
			(setq v (undo-tree-make-visualizer-data)))))
     (setf (undo-tree-visualizer-data-rwidth v) ,val)))

(defsetf undo-tree-node-marker (node) (val)
  `(let ((v (plist-get (undo-tree-node-meta-data ,node) :visualizer)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-meta-data ,node)
	     (plist-put (undo-tree-node-meta-data ,node) :visualizer
			(setq v (undo-tree-make-visualizer-data)))))
     (setf (undo-tree-visualizer-data-marker v) ,val)))



(defstruct
  (undo-tree-register-data
   (:type vector)
   (:constructor nil)
   (:constructor undo-tree-make-register-data (buffer node)))
  buffer node)

(defun undo-tree-register-data-p (data)
  (and (vectorp data)
       (= (length data) 2)
       (undo-tree-node-p (undo-tree-register-data-node data))))

(defun undo-tree-register-data-print-func (data)
  (princ (format "an undo-tree state for buffer %s"
		 (undo-tree-register-data-buffer data))))

(defmacro undo-tree-node-register (node)
  `(plist-get (undo-tree-node-meta-data ,node) :register))

(defsetf undo-tree-node-register (node) (val)
  `(setf (undo-tree-node-meta-data ,node)
	 (plist-put (undo-tree-node-meta-data ,node) :register ,val)))




;;; =====================================================================
;;;              Basic undo-tree data structure functions

(defun undo-tree-grow (undo)
  "Add an UNDO node to current branch of `buffer-undo-tree'."
  (let* ((current (undo-tree-current buffer-undo-tree))
         (new (undo-tree-make-node current undo)))
    (push new (undo-tree-node-next current))
    (setf (undo-tree-current buffer-undo-tree) new)))


(defun undo-tree-grow-backwards (node undo &optional redo)
  "Add new node *above* undo-tree NODE, and return new node.
Note that this will overwrite NODE's \"previous\" link, so should
only be used on a detached NODE, never on nodes that are already
part of `buffer-undo-tree'."
  (let ((new (undo-tree-make-node-backwards node undo redo)))
    (setf (undo-tree-node-previous node) new)
    new))


(defun undo-tree-splice-node (node splice)
  "Splice NODE into undo tree, below node SPLICE.
Note that this will overwrite NODE's \"next\" and \"previous\"
links, so should only be used on a detached NODE, never on nodes
that are already part of `buffer-undo-tree'."
  (setf (undo-tree-node-next node) (undo-tree-node-next splice)
	(undo-tree-node-branch node) (undo-tree-node-branch splice)
	(undo-tree-node-previous node) splice
	(undo-tree-node-next splice) (list node)
	(undo-tree-node-branch splice) 0)
  (dolist (n (undo-tree-node-next node))
    (setf (undo-tree-node-previous n) node)))


(defun undo-tree-snip-node (node)
  "Snip NODE out of undo tree."
  (let* ((parent (undo-tree-node-previous node))
	 position p)
    ;; if NODE is only child, replace parent's next links with NODE's
    (if (= (length (undo-tree-node-next parent)) 0)
	(setf (undo-tree-node-next parent) (undo-tree-node-next node)
	      (undo-tree-node-branch parent) (undo-tree-node-branch node))
      ;; otherwise...
      (setq position (undo-tree-position node (undo-tree-node-next parent)))
      (cond
       ;; if active branch used do go via NODE, set parent's branch to active
       ;; branch of NODE
       ((= (undo-tree-node-branch parent) position)
	(setf (undo-tree-node-branch parent)
	      (+ position (undo-tree-node-branch node))))
       ;; if active branch didn't go via NODE, update parent's branch to point
       ;; to same node as before
       ((> (undo-tree-node-branch parent) position)
	(incf (undo-tree-node-branch parent)
	      (1- (length (undo-tree-node-next node))))))
      ;; replace NODE in parent's next list with NODE's entire next list
      (if (= position 0)
	  (setf (undo-tree-node-next parent)
		(nconc (undo-tree-node-next node)
		       (cdr (undo-tree-node-next parent))))
	(setq p (nthcdr (1- position) (undo-tree-node-next parent)))
	(setcdr p (nconc (undo-tree-node-next node) (cddr p)))))
    ;; update previous links of NODE's children
    (dolist (n (undo-tree-node-next node))
      (setf (undo-tree-node-previous n) parent))))


(defun undo-tree-mapc (--undo-tree-mapc-function-- undo-tree)
  ;; Apply FUNCTION to each node in UNDO-TREE.
  (let ((stack (list (undo-tree-root undo-tree)))
        node)
    (while stack
      (setq node (pop stack))
      (funcall --undo-tree-mapc-function-- node)
      (setq stack (append (undo-tree-node-next node) stack)))))


(defmacro undo-tree-num-branches ()
  "Return number of branches at current undo tree node."
  '(length (undo-tree-node-next (undo-tree-current buffer-undo-tree))))


(defun undo-tree-position (node list)
  "Find the first occurrence of NODE in LIST.
Return the index of the matching item, or nil of not found.
Comparison is done with `eq'."
  (let ((i 0))
    (catch 'found
      (while (progn
               (when (eq node (car list)) (throw 'found i))
               (incf i)
               (setq list (cdr list))))
      nil)))


(defvar *undo-tree-id-counter* 0)
(make-variable-buffer-local '*undo-tree-id-counter*)

(defmacro undo-tree-generate-id ()
  ;; Generate a new, unique id (uninterned symbol).
  ;; The name is made by appending a number to "undo-tree-id".
  ;; (Copied from CL package `gensym'.)
  `(let ((num (prog1 *undo-tree-id-counter* (incf *undo-tree-id-counter*))))
     (make-symbol (format "undo-tree-id%d" num))))




;;; =====================================================================
;;;    Utility functions for handling `buffer-undo-list' and changesets

(defmacro undo-list-marker-elt-p (elt)
  `(markerp (car-safe ,elt)))

(defmacro undo-list-GCd-marker-elt-p (elt)
  ;; Return t if ELT is a marker element whose marker has been moved to the
  ;; object-pool, so may potentially have been garbage-collected.
  ;; Note: Valid marker undo elements should be uniquely identified as cons
  ;; cells with a symbol in the car (replacing the marker), and a number in
  ;; the cdr. However, to guard against future changes to undo element
  ;; formats, we perform an additional redundant check on the symbol name.
  `(and (car-safe ,elt)
	(symbolp (car ,elt))
	(let ((str (symbol-name (car ,elt))))
	  (and (> (length str) 12)
	       (string= (substring str 0 12) "undo-tree-id")))
	(numberp (cdr-safe ,elt))))


(defun undo-tree-move-GC-elts-to-pool (elt)
  ;; Move elements that can be garbage-collected into `buffer-undo-tree'
  ;; object pool, substituting a unique id that can be used to retrieve them
  ;; later. (Only markers require this treatment currently.)
  (when (undo-list-marker-elt-p elt)
    (let ((id (undo-tree-generate-id)))
      (puthash id (car elt) (undo-tree-object-pool buffer-undo-tree))
      (setcar elt id))))


(defun undo-tree-restore-GC-elts-from-pool (elt)
  ;; Replace object id's in ELT with corresponding objects from
  ;; `buffer-undo-tree' object pool and return modified ELT, or return nil if
  ;; any object in ELT has been garbage-collected.
  (if (undo-list-GCd-marker-elt-p elt)
      (when (setcar elt (gethash (car elt)
				 (undo-tree-object-pool buffer-undo-tree)))
	elt)
    elt))


(defun undo-list-clean-GCd-elts (undo-list)
  ;; Remove object id's from UNDO-LIST that refer to elements that have been
  ;; garbage-collected. UNDO-LIST is modified by side-effect.
  (while (undo-list-GCd-marker-elt-p (car undo-list))
    (unless (gethash (caar undo-list)
		     (undo-tree-object-pool buffer-undo-tree))
      (setq undo-list (cdr undo-list))))
  (let ((p undo-list))
    (while (cdr p)
      (when (and (undo-list-GCd-marker-elt-p (cadr p))
		 (null (gethash (car (cadr p))
				(undo-tree-object-pool buffer-undo-tree))))
	(setcdr p (cddr p)))
      (setq p (cdr p))))
  undo-list)


(defun undo-list-pop-changeset (&optional discard-pos)
  ;; Pop changeset from `buffer-undo-list'. If DISCARD-POS is non-nil, discard
  ;; any position entries from changeset.

  ;; discard undo boundaries and (if DISCARD-POS is non-nil) position entries
  ;; at head of undo list
  (while (or (null (car buffer-undo-list))
	     (and discard-pos (integerp (car buffer-undo-list))))
    (setq buffer-undo-list (cdr buffer-undo-list)))
  ;; pop elements up to next undo boundary, discarding position entries if
  ;; DISCARD-POS is non-nil
  (if (eq (car buffer-undo-list) 'undo-tree-canary)
      (push nil buffer-undo-list)
    (let* ((changeset (list (pop buffer-undo-list)))
           (p changeset))
      (while (progn
	       (undo-tree-move-GC-elts-to-pool (car p))
	       (while (and discard-pos (integerp (car buffer-undo-list)))
		 (setq buffer-undo-list (cdr buffer-undo-list)))
	       (car buffer-undo-list))
        (setcdr p (list (pop buffer-undo-list)))
	(setq p (cdr p)))
      changeset)))


(defun undo-tree-copy-list (undo-list)
  ;; Return a deep copy of first changeset in `undo-list'. Object id's are
  ;; replaced by corresponding objects from `buffer-undo-tree' object-pool.
  (when undo-list
    (let (copy p)
      ;; if first element contains an object id, replace it with object from
      ;; pool, discarding element entirely if it's been GC'd
      (while (null copy)
	(setq copy
	      (undo-tree-restore-GC-elts-from-pool (pop undo-list))))
      (setq copy (list copy)
	    p copy)
      ;; copy remaining elements, replacing object id's with objects from
      ;; pool, or discarding them entirely if they've been GC'd
      (while undo-list
	(when (setcdr p (undo-tree-restore-GC-elts-from-pool
			 (undo-copy-list-1 (pop undo-list))))
	  (setcdr p (list (cdr p)))
	  (setq p (cdr p))))
      copy)))



(defun undo-list-transfer-to-tree ()
  ;; Transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'.

  ;; if `buffer-undo-tree' is empty, create initial undo-tree
  (when (null buffer-undo-tree) (setq buffer-undo-tree (make-undo-tree)))
  ;; make sure there's a canary at end of `buffer-undo-list'
  (when (null buffer-undo-list)
    (setq buffer-undo-list '(nil undo-tree-canary)))

  (unless (eq (cadr buffer-undo-list) 'undo-tree-canary)
    ;; create new node from first changeset in `buffer-undo-list', save old
    ;; `buffer-undo-tree' current node, and make new node the current node
    (let* ((node (undo-tree-make-node nil (undo-list-pop-changeset)))
	   (splice (undo-tree-current buffer-undo-tree))
	   (size (undo-list-byte-size (undo-tree-node-undo node))))
      (setf (undo-tree-current buffer-undo-tree) node)
      ;; grow tree fragment backwards using `buffer-undo-list' changesets
      (while (and buffer-undo-list
		  (not (eq (cadr buffer-undo-list) 'undo-tree-canary)))
	(setq node
	      (undo-tree-grow-backwards node (undo-list-pop-changeset)))
	(incf size (undo-list-byte-size (undo-tree-node-undo node))))
      ;; if no undo history has been discarded from `buffer-undo-list' since
      ;; last transfer, splice new tree fragment onto end of old
      ;; `buffer-undo-tree' current node
      (if (eq (cadr buffer-undo-list) 'undo-tree-canary)
	  (progn
	    (setf (undo-tree-node-previous node) splice)
	    (push node (undo-tree-node-next splice))
	    (setf (undo-tree-node-branch splice) 0)
	    (incf (undo-tree-size buffer-undo-tree) size))
	;; if undo history has been discarded, replace entire
	;; `buffer-undo-tree' with new tree fragment
	(setq node (undo-tree-grow-backwards node nil))
	(setf (undo-tree-root buffer-undo-tree) node)
	(setq buffer-undo-list '(nil undo-tree-canary))
	(setf (undo-tree-size buffer-undo-tree) size)
	(setq buffer-undo-list '(nil undo-tree-canary))))
    ;; discard undo history if necessary
    (undo-tree-discard-history)))


(defun undo-list-byte-size (undo-list)
  ;; Return size (in bytes) of UNDO-LIST
  (let ((size 0) (p undo-list))
    (while p
      (incf size 8)  ; cons cells use up 8 bytes
      (when (and (consp (car p)) (stringp (caar p)))
        (incf size (string-bytes (caar p))))
      (setq p (cdr p)))
    size))



(defun undo-list-rebuild-from-tree ()
  "Rebuild `buffer-undo-list' from information in `buffer-undo-tree'."
  (unless (eq buffer-undo-list t)
    (undo-list-transfer-to-tree)
    (setq buffer-undo-list nil)
    (when buffer-undo-tree
      (let ((stack (list (list (undo-tree-root buffer-undo-tree)))))
	(push (sort (mapcar 'identity (undo-tree-node-next (caar stack)))
		    (lambda (a b)
		      (time-less-p (undo-tree-node-timestamp a)
				   (undo-tree-node-timestamp b))))
	      stack)
	;; Traverse tree in depth-and-oldest-first order, but add undo records
	;; on the way down, and redo records on the way up.
	(while (or (car stack)
		   (not (eq (car (nth 1 stack))
			    (undo-tree-current buffer-undo-tree))))
	  (if (car stack)
	      (progn
		(setq buffer-undo-list
		      (append (undo-tree-node-undo (caar stack))
			      buffer-undo-list))
		(undo-boundary)
		(push (sort (mapcar 'identity
				    (undo-tree-node-next (caar stack)))
			    (lambda (a b)
			      (time-less-p (undo-tree-node-timestamp a)
					   (undo-tree-node-timestamp b))))
		      stack))
	    (pop stack)
	    (setq buffer-undo-list
		  (append (undo-tree-node-redo (caar stack))
			  buffer-undo-list))
	    (undo-boundary)
	    (pop (car stack))))))))




;;; =====================================================================
;;;                  History discarding functions

(defun undo-tree-oldest-leaf (node)
  ;; Return oldest leaf node below NODE.
  (while (undo-tree-node-next node)
    (setq node
          (car (sort (mapcar 'identity (undo-tree-node-next node))
                     (lambda (a b)
                       (time-less-p (undo-tree-node-timestamp a)
                                    (undo-tree-node-timestamp b)))))))
  node)


(defun undo-tree-discard-node (node)
  ;; Discard NODE from `buffer-undo-tree', and return next in line for
  ;; discarding.

  ;; don't discard current node
  (unless (eq node (undo-tree-current buffer-undo-tree))

    ;; discarding root node...
    (if (eq node (undo-tree-root buffer-undo-tree))
        (cond
         ;; should always discard branches before root
         ((> (length (undo-tree-node-next node)) 1)
          (error "Trying to discard undo-tree root which still\
 has multiple branches"))
         ;; don't discard root if current node is only child
         ((eq (car (undo-tree-node-next node))
              (undo-tree-current buffer-undo-tree))
	  nil)
	 ;; discard root
         (t
	  ;; clear any register referring to root
	  (let ((r (undo-tree-node-register node)))
	    (when (and r (eq (get-register r) node))
	      (set-register r nil)))
          ;; make child of root into new root
          (setq node (setf (undo-tree-root buffer-undo-tree)
                           (car (undo-tree-node-next node))))
	  ;; update undo-tree size
	  (decf (undo-tree-size buffer-undo-tree)
		(+ (undo-list-byte-size (undo-tree-node-undo node))
		   (undo-list-byte-size (undo-tree-node-redo node))))
	  ;; discard new root's undo data
	  (setf (undo-tree-node-undo node) nil
		(undo-tree-node-redo node) nil)
          ;; if new root has branches, or new root is current node, next node
          ;; to discard is oldest leaf, otherwise it's new root
          (if (or (> (length (undo-tree-node-next node)) 1)
                  (eq (car (undo-tree-node-next node))
                      (undo-tree-current buffer-undo-tree)))
              (undo-tree-oldest-leaf node)
            node)))

      ;; discarding leaf node...
      (let* ((parent (undo-tree-node-previous node))
             (current (nth (undo-tree-node-branch parent)
                           (undo-tree-node-next parent))))
	;; clear any register referring to the discarded node
	(let ((r (undo-tree-node-register node)))
	  (when (and r (eq (get-register r) node))
	    (set-register r nil)))
	;; update undo-tree size
	(decf (undo-tree-size buffer-undo-tree)
	      (+ (undo-list-byte-size (undo-tree-node-undo node))
		 (undo-list-byte-size (undo-tree-node-redo node))))
        (setf (undo-tree-node-next parent)
                (delq node (undo-tree-node-next parent))
              (undo-tree-node-branch parent)
                (undo-tree-position current (undo-tree-node-next parent)))
        ;; if parent has branches, or parent is current node, next node to
        ;; discard is oldest leaf, otherwise it's parent
        (if (or (eq parent (undo-tree-current buffer-undo-tree))
                (and (undo-tree-node-next parent)
                     (or (not (eq parent (undo-tree-root buffer-undo-tree)))
                         (> (length (undo-tree-node-next parent)) 1))))
            (undo-tree-oldest-leaf parent)
          parent)))))



(defun undo-tree-discard-history ()
  "Discard undo history until we're within memory usage limits
set by `undo-limit', `undo-strong-limit' and `undo-outer-limit'."

  (when (> (undo-tree-size buffer-undo-tree) undo-limit)
    ;; if there are no branches off root, first node to discard is root;
    ;; otherwise it's leaf node at botom of oldest branch
    (let ((node (if (> (length (undo-tree-node-next
                                (undo-tree-root buffer-undo-tree))) 1)
                    (undo-tree-oldest-leaf (undo-tree-root buffer-undo-tree))
                  (undo-tree-root buffer-undo-tree))))

      ;; discard nodes until memory use is within `undo-strong-limit'
      (while (and node
                  (> (undo-tree-size buffer-undo-tree) undo-strong-limit))
        (setq node (undo-tree-discard-node node)))

      ;; discard nodes until next node to discard would bring memory use
      ;; within `undo-limit'
      (while (and node
		  ;; check first if last discard has brought us within
		  ;; `undo-limit', in case we can avoid more expensive
		  ;; `undo-strong-limit' calculation
		  ;; Note: this assumes undo-strong-limit > undo-limit;
		  ;;       if not, effectively undo-strong-limit = undo-limit
		  (> (undo-tree-size buffer-undo-tree) undo-limit)
                  (> (- (undo-tree-size buffer-undo-tree)
			;; if next node to discard is root, the memory we
			;; free-up comes from discarding changesets from its
			;; only child...
			(if (eq node (undo-tree-root buffer-undo-tree))
			    (+ (undo-list-byte-size
				(undo-tree-node-undo
				 (car (undo-tree-node-next node))))
			       (undo-list-byte-size
				(undo-tree-node-redo
				 (car (undo-tree-node-next node)))))
			  ;; ...otherwise, it comes from discarding changesets
			  ;; from along with the node itself
			  (+ (undo-list-byte-size (undo-tree-node-undo node))
			     (undo-list-byte-size (undo-tree-node-redo node)))
			  ))
                     undo-limit))
        (setq node (undo-tree-discard-node node)))

      ;; if we're still over the `undo-outer-limit', discard entire history
      (when (> (undo-tree-size buffer-undo-tree) undo-outer-limit)
        ;; query first if `undo-ask-before-discard' is set
        (if undo-ask-before-discard
            (when (yes-or-no-p
                   (format
                    "Buffer `%s' undo info is %d bytes long;  discard it? "
                    (buffer-name) (undo-tree-size buffer-undo-tree)))
              (setq buffer-undo-tree nil))
          ;; otherwise, discard and display warning
          (display-warning
           '(undo discard-info)
           (concat
            (format "Buffer `%s' undo info was %d bytes long.\n"
                    (buffer-name) (undo-tree-size buffer-undo-tree))
            "The undo info was discarded because it exceeded\
 `undo-outer-limit'.

This is normal if you executed a command that made a huge change
to the buffer. In that case, to prevent similar problems in the
future, set `undo-outer-limit' to a value that is large enough to
cover the maximum size of normal changes you expect a single
command to make, but not so large that it might exceed the
maximum memory allotted to Emacs.

If you did not execute any such command, the situation is
probably due to a bug and you should report it.

You can disable the popping up of this buffer by adding the entry
\(undo discard-info) to the user option `warning-suppress-types',
which is defined in the `warnings' library.\n")
           :warning)
          (setq buffer-undo-tree nil)))
      )))




;;; =====================================================================
;;;                 Visualizer-related functions

(defun undo-tree-compute-widths (undo-tree)
  "Recursively compute widths for all UNDO-TREE's nodes."
  (let ((stack (list (undo-tree-root undo-tree)))
        res)
    (while stack
      ;; try to compute widths for node at top of stack
      (if (undo-tree-node-p
           (setq res (undo-tree-node-compute-widths (car stack))))
          ;; if computation fails, it returns a node whose widths still need
          ;; computing, which we push onto the stack
          (push res stack)
        ;; otherwise, store widths and remove it from stack
        (setf (undo-tree-node-lwidth (car stack)) (aref res 0)
              (undo-tree-node-cwidth (car stack)) (aref res 1)
              (undo-tree-node-rwidth (car stack)) (aref res 2))
        (pop stack)))))


(defun undo-tree-node-compute-widths (node)
  ;; Compute NODE's left-, centre-, and right-subtree widths. Returns widths
  ;; (in a vector) if successful. Otherwise, returns a node whose widths need
  ;; calculating before NODE's can be calculated.
  (let ((num-children (length (undo-tree-node-next node)))
        (lwidth 0) (cwidth 0) (rwidth 0)
        p w)
    (catch 'need-widths
      (cond
       ;; leaf nodes have 0 width
       ((= 0 num-children)
        (setf cwidth 1
              (undo-tree-node-lwidth node) 0
              (undo-tree-node-cwidth node) 1
              (undo-tree-node-rwidth node) 0))

       ;; odd number of children
       ((= (mod num-children 2) 1)
        (setq p (undo-tree-node-next node))
        ;; compute left-width
        (dotimes (i (/ num-children 2))
          (if (undo-tree-node-lwidth (car p))
              (incf lwidth (+ (undo-tree-node-lwidth (car p))
                              (undo-tree-node-cwidth (car p))
                              (undo-tree-node-rwidth (car p))))
            ;; if child's widths haven't been computed, return that child
            (throw 'need-widths (car p)))
          (setq p (cdr p)))
        (if (undo-tree-node-lwidth (car p))
            (incf lwidth (undo-tree-node-lwidth (car p)))
          (throw 'need-widths (car p)))
        ;; centre-width is inherited from middle child
        (setf cwidth (undo-tree-node-cwidth (car p)))
        ;; compute right-width
        (incf rwidth (undo-tree-node-rwidth (car p)))
        (setq p (cdr p))
        (dotimes (i (/ num-children 2))
          (if (undo-tree-node-lwidth (car p))
              (incf rwidth (+ (undo-tree-node-lwidth (car p))
                              (undo-tree-node-cwidth (car p))
                              (undo-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p))))

       ;; even number of children
       (t
        (setq p (undo-tree-node-next node))
        ;; compute left-width
        (dotimes (i (/ num-children 2))
          (if (undo-tree-node-lwidth (car p))
              (incf lwidth (+ (undo-tree-node-lwidth (car p))
                              (undo-tree-node-cwidth (car p))
                              (undo-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p)))
        ;; centre-width is 0 when number of children is even
        (setq cwidth 0)
        ;; compute right-width
        (dotimes (i (/ num-children 2))
          (if (undo-tree-node-lwidth (car p))
              (incf rwidth (+ (undo-tree-node-lwidth (car p))
                              (undo-tree-node-cwidth (car p))
                              (undo-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p)))))

      ;; return left-, centre- and right-widths
      (vector lwidth cwidth rwidth))))


(defun undo-tree-clear-visualizer-data (undo-tree)
  ;; Clear visualizer data from UNDO-TREE.
  (undo-tree-mapc
   (lambda (node) (undo-tree-node-clear-visualizer-data node))
   undo-tree))




;;; =====================================================================
;;;                     Undo-in-region functions

(defun undo-tree-pull-undo-in-region-branch (start end)
  ;; Pull out entries from undo changesets to create a new undo-in-region
  ;; branch, which undoes changeset entries lying between START and END first,
  ;; followed by remaining entries from the changesets, before rejoining the
  ;; existing undo tree history. Repeated calls will, if appropriate, extend
  ;; the current undo-in-region branch rather than creating a new one.

  ;; if we're just reverting the last redo-in-region, we don't need to
  ;; manipulate the undo tree at all
  (if (undo-tree-reverting-redo-in-region-p start end)
      t  ; return t to indicate success

    ;; We build the `region-changeset' and `delta-list' lists forwards, using
    ;; pointers `r' and `d' to the penultimate element of the list. So that we
    ;; don't have to treat the first element differently, we prepend a dummy
    ;; leading nil to the lists, and have the pointers point to that
    ;; initially.
    ;; Note: using '(nil) instead of (list nil) in the `let*' results in
    ;;       bizarre errors when the code is byte-compiled, where parts of the
    ;;       lists appear to survive across different calls to this function.
    ;;       An obscure byte-compiler bug, perhaps?
    (let* ((region-changeset (list nil))
	   (r region-changeset)
	   (delta-list (list nil))
	   (d delta-list)
	   (node (undo-tree-current buffer-undo-tree))
	   (repeated-undo-in-region
	    (undo-tree-repeated-undo-in-region-p start end))
	   undo-adjusted-markers  ; `undo-elt-in-region' expects this
	   fragment splice original-fragment original-splice original-current
	   got-visible-elt undo-list elt)

      ;; --- initialisation ---
      (cond
       ;; if this is a repeated undo in the same region, start pulling changes
       ;; from NODE at which undo-in-region branch iss attached, and detatch
       ;; the branch, using it as initial FRAGMENT of branch being constructed
       (repeated-undo-in-region
	(setq original-current node
	      fragment (car (undo-tree-node-next node))
	      splice node)
	;; undo up to node at which undo-in-region branch is attached
	;; (recognizable as first node with more than one branch)
	(let ((mark-active nil))
	  (while (= (length (undo-tree-node-next node)) 1)
	    (undo-tree-undo-1)
	    (setq fragment node
		  node (undo-tree-current buffer-undo-tree))))
	(when (eq splice node) (setq splice nil))
	;; detatch undo-in-region branch
	(setf (undo-tree-node-next node)
	      (delq fragment (undo-tree-node-next node))
	      (undo-tree-node-previous fragment) nil
	      original-fragment fragment
	      original-splice node))

       ;; if this is a new undo-in-region, initial FRAGMENT is a copy of all
       ;; nodes below the current one in the active branch
       ((undo-tree-node-next node)
	(setq fragment (undo-tree-make-node nil nil)
	      splice fragment)
	(while (setq node (nth (undo-tree-node-branch node)
			       (undo-tree-node-next node)))
	  (push (undo-tree-make-node
		 splice
		 (undo-copy-list (undo-tree-node-undo node))
		 (undo-copy-list (undo-tree-node-redo node)))
		(undo-tree-node-next splice))
	  (setq splice (car (undo-tree-node-next splice))))
	(setq fragment (car (undo-tree-node-next fragment))
	      splice nil
	      node (undo-tree-current buffer-undo-tree))))


      ;; --- pull undo-in-region elements into branch ---
      ;; work backwards up tree, pulling out undo elements within region until
      ;; we've got one that undoes a visible change (insertion or deletion)
      (catch 'abort
	(while (and (not got-visible-elt) node (undo-tree-node-undo node))
	  ;; we cons a dummy nil element on the front of the changeset so that
	  ;; we can conveniently remove the first (real) element from the
	  ;; changeset if we need to; the leading nil is removed once we're
	  ;; done with this changeset
	  (setq undo-list (cons nil (undo-copy-list (undo-tree-node-undo node)))
		elt (cadr undo-list))
	  (if fragment
	      (progn
		(setq fragment (undo-tree-grow-backwards fragment undo-list))
		(unless splice (setq splice fragment)))
	    (setq fragment (undo-tree-make-node nil undo-list))
	    (setq splice fragment))

	  (while elt
	    (cond
	     ;; keep elements within region
	     ((undo-elt-in-region elt start end)
	      ;; set flag if kept element is visible (insertion or deletion)
	      (when (and (consp elt)
			 (or (stringp (car elt)) (integerp (car elt))))
		(setq got-visible-elt t))
	      ;; adjust buffer positions in elements previously undone before
	      ;; kept element, as kept element will now be undone first
	      (undo-tree-adjust-elements-to-elt splice elt)
	      ;; move kept element to undo-in-region changeset, adjusting its
	      ;; buffer position as it will now be undone first
	      (setcdr r (list (undo-tree-apply-deltas elt (cdr delta-list))))
	      (setq r (cdr r))
	      (setcdr undo-list (cddr undo-list)))

	     ;; discard "was unmodified" elements
	     ;; FIXME: deal properly with these
	     ((and (consp elt) (eq (car elt) t))
	      (setcdr undo-list (cddr undo-list)))

	     ;; if element crosses region, we can't pull any more elements
	     ((undo-elt-crosses-region elt start end)
	      ;; if we've found a visible element, it must be earlier in
	      ;; current node's changeset; stop pulling elements (null
	      ;; `undo-list' and non-nil `got-visible-elt' cause loop to exit)
	      (if got-visible-elt
		  (setq undo-list nil)
		;; if we haven't found a visible element yet, pulling
		;; undo-in-region branch has failed
		(setq region-changeset nil)
		(throw 'abort t)))

	     ;; if rejecting element, add its delta (if any) to the list
	     (t
	      (let ((delta (undo-delta elt)))
		(when (/= 0 (cdr delta))
		  (setcdr d (list delta))
		  (setq d (cdr d))))
	      (setq undo-list (cdr undo-list))))

	    ;; process next element of current changeset
	    (setq elt (cadr undo-list)))

	  ;; if there are remaining elements in changeset, remove dummy nil
	  ;; from front
	  (if (cadr (undo-tree-node-undo fragment))
	      (pop (undo-tree-node-undo fragment))
	    ;; otherwise, if we've kept all elements in changeset, discard
	    ;; empty changeset
	    (when (eq splice fragment) (setq splice nil))
	    (setq fragment (car (undo-tree-node-next fragment))))
	  ;; process changeset from next node up the tree
	  (setq node (undo-tree-node-previous node))))

      ;; pop dummy nil from front of `region-changeset'
      (pop region-changeset)


      ;; --- integrate branch into tree ---
      ;; if no undo-in-region elements were found, restore undo tree
      (if (null region-changeset)
	  (when original-current
	    (push original-fragment (undo-tree-node-next original-splice))
	    (setf (undo-tree-node-branch original-splice) 0
		  (undo-tree-node-previous original-fragment) original-splice)
	    (let ((mark-active nil))
	      (while (not (eq (undo-tree-current buffer-undo-tree)
			      original-current))
		(undo-tree-redo-1)))
	    nil)  ; return nil to indicate failure

	;; otherwise...
	;; need to undo up to node where new branch will be attached, to
	;; ensure redo entries are populated, and then redo back to where we
	;; started
	(let ((mark-active nil)
	      (current (undo-tree-current buffer-undo-tree)))
	  (while (not (eq (undo-tree-current buffer-undo-tree) node))
	    (undo-tree-undo-1))
	  (while (not (eq (undo-tree-current buffer-undo-tree) current))
	    (undo-tree-redo-1)))

	(cond
	 ;; if there's no remaining fragment, just create undo-in-region node
	 ;; and attach it to parent of last node from which elements were
	 ;; pulled
	 ((null fragment)
	  (setq fragment (undo-tree-make-node node region-changeset))
	  (push fragment (undo-tree-node-next node))
	  (setf (undo-tree-node-branch node) 0)
	  ;; set current node to undo-in-region node
	  (setf (undo-tree-current buffer-undo-tree) fragment))

	 ;; if no splice point has been set, add undo-in-region node to top of
	 ;; fragment and attach it to parent of last node from which elements
	 ;; were pulled
	 ((null splice)
	  (setq fragment (undo-tree-grow-backwards fragment region-changeset))
	  (push fragment (undo-tree-node-next node))
	  (setf (undo-tree-node-branch node) 0
		(undo-tree-node-previous fragment) node)
	  ;; set current node to undo-in-region node
	  (setf (undo-tree-current buffer-undo-tree) fragment))

	 ;; if fragment contains nodes, attach fragment to parent of last node
	 ;; from which elements were pulled, and splice in undo-in-region node
	 (t
	  (setf (undo-tree-node-previous fragment) node)
	  (push fragment (undo-tree-node-next node))
	  (setf (undo-tree-node-branch node) 0)
	  ;; if this is a repeated undo-in-region, then we've left the current
	  ;; node at the original splice-point; we need to set the current
	  ;; node to the equivalent node on the undo-in-region branch and redo
	  ;; back to where we started
	  (when repeated-undo-in-region
	    (setf (undo-tree-current buffer-undo-tree)
		  (undo-tree-node-previous original-fragment))
	    (let ((mark-active nil))
	      (while (not (eq (undo-tree-current buffer-undo-tree) splice))
		(undo-tree-redo-1 nil 'preserve-undo))))
	  ;; splice new undo-in-region node into fragment
	  (setq node (undo-tree-make-node nil region-changeset))
	  (undo-tree-splice-node node splice)
	  ;; set current node to undo-in-region node
	  (setf (undo-tree-current buffer-undo-tree) node)))

	;; update undo-tree size
	(setq node (undo-tree-node-previous fragment))
	(while (progn
		 (and (setq node (car (undo-tree-node-next node)))
		      (not (eq node original-fragment))
		      (incf (undo-tree-size buffer-undo-tree)
			    (undo-list-byte-size (undo-tree-node-undo node)))
		      (when (undo-tree-node-redo node)
			(incf (undo-tree-size buffer-undo-tree)
			      (undo-list-byte-size (undo-tree-node-redo node))))
		      )))
	t)  ; indicate undo-in-region branch was successfully pulled
      )))



(defun undo-tree-pull-redo-in-region-branch (start end)
  ;; Pull out entries from redo changesets to create a new redo-in-region
  ;; branch, which redoes changeset entries lying between START and END first,
  ;; followed by remaining entries from the changesets. Repeated calls will,
  ;; if appropriate, extend the current redo-in-region branch rather than
  ;; creating a new one.

  ;; if we're just reverting the last undo-in-region, we don't need to
  ;; manipulate the undo tree at all
  (if (undo-tree-reverting-undo-in-region-p start end)
      t  ; return t to indicate success

    ;; We build the `region-changeset' and `delta-list' lists forwards, using
    ;; pointers `r' and `d' to the penultimate element of the list. So that we
    ;; don't have to treat the first element differently, we prepend a dummy
    ;; leading nil to the lists, and have the pointers point to that
    ;; initially.
    ;; Note: using '(nil) instead of (list nil) in the `let*' causes bizarre
    ;;       errors when the code is byte-compiled, where parts of the lists
    ;;       appear to survive across different calls to this function.  An
    ;;       obscure byte-compiler bug, perhaps?
    (let* ((region-changeset (list nil))
	   (r region-changeset)
	   (delta-list (list nil))
	   (d delta-list)
	   (node (undo-tree-current buffer-undo-tree))
	   (repeated-redo-in-region
	    (undo-tree-repeated-redo-in-region-p start end))
	   undo-adjusted-markers  ; `undo-elt-in-region' expects this
	   fragment splice got-visible-elt redo-list elt)

      ;; --- inisitalisation ---
      (cond
       ;; if this is a repeated redo-in-region, detach fragment below current
       ;; node
       (repeated-redo-in-region
	(when (setq fragment (car (undo-tree-node-next node)))
	  (setf (undo-tree-node-previous fragment) nil
		(undo-tree-node-next node)
		(delq fragment (undo-tree-node-next node)))))
       ;; if this is a new redo-in-region, initial fragment is a copy of all
       ;; nodes below the current one in the active branch
       ((undo-tree-node-next node)
	(setq fragment (undo-tree-make-node nil nil)
	      splice fragment)
	(while (setq node (nth (undo-tree-node-branch node)
			       (undo-tree-node-next node)))
	  (push (undo-tree-make-node
		 splice nil
		 (undo-copy-list (undo-tree-node-redo node)))
		(undo-tree-node-next splice))
	  (setq splice (car (undo-tree-node-next splice))))
	(setq fragment (car (undo-tree-node-next fragment)))))


      ;; --- pull redo-in-region elements into branch ---
      ;; work down fragment, pulling out redo elements within region until
      ;; we've got one that redoes a visible change (insertion or deletion)
      (setq node fragment)
      (catch 'abort
	(while (and (not got-visible-elt) node (undo-tree-node-redo node))
	  ;; we cons a dummy nil element on the front of the changeset so that
	  ;; we can conveniently remove the first (real) element from the
	  ;; changeset if we need to; the leading nil is removed once we're
	  ;; done with this changeset
	  (setq redo-list (push nil (undo-tree-node-redo node))
		elt (cadr redo-list))
	  (while elt
	    (cond
	     ;; keep elements within region
	     ((undo-elt-in-region elt start end)
	      ;; set flag if kept element is visible (insertion or deletion)
	      (when (and (consp elt)
			 (or (stringp (car elt)) (integerp (car elt))))
		(setq got-visible-elt t))
	      ;; adjust buffer positions in elements previously redone before
	      ;; kept element, as kept element will now be redone first
	      (undo-tree-adjust-elements-to-elt fragment elt t)
	      ;; move kept element to redo-in-region changeset, adjusting its
	      ;; buffer position as it will now be redone first
	      (setcdr r (list (undo-tree-apply-deltas elt (cdr delta-list) -1)))
	      (setq r (cdr r))
	      (setcdr redo-list (cddr redo-list)))

	     ;; discard "was unmodified" elements
	     ;; FIXME: deal properly with these
	     ((and (consp elt) (eq (car elt) t))
	      (setcdr redo-list (cddr redo-list)))

	     ;; if element crosses region, we can't pull any more elements
	     ((undo-elt-crosses-region elt start end)
	      ;; if we've found a visible element, it must be earlier in
	      ;; current node's changeset; stop pulling elements (null
	      ;; `redo-list' and non-nil `got-visible-elt' cause loop to exit)
	      (if got-visible-elt
		  (setq redo-list nil)
		;; if we haven't found a visible element yet, pulling
		;; redo-in-region branch has failed
		(setq region-changeset nil)
		(throw 'abort t)))

	     ;; if rejecting element, add its delta (if any) to the list
	     (t
	      (let ((delta (undo-delta elt)))
		(when (/= 0 (cdr delta))
		  (setcdr d (list delta))
		  (setq d (cdr d))))
	      (setq redo-list (cdr redo-list))))

	    ;; process next element of current changeset
	    (setq elt (cadr redo-list)))

	  ;; if there are remaining elements in changeset, remove dummy nil
	  ;; from front
	  (if (cadr (undo-tree-node-redo node))
	      (pop (undo-tree-node-undo node))
	    ;; otherwise, if we've kept all elements in changeset, discard
	    ;; empty changeset
	    (if (eq fragment node)
		(setq fragment (car (undo-tree-node-next fragment)))
	      (undo-tree-snip-node node)))
	  ;; process changeset from next node in fragment
	  (setq node (car (undo-tree-node-next node)))))

      ;; pop dummy nil from front of `region-changeset'
      (pop region-changeset)


      ;; --- integrate branch into tree ---
      (setq node (undo-tree-current buffer-undo-tree))
      ;; if no redo-in-region elements were found, restore undo tree
      (if (null (car region-changeset))
	  (when (and repeated-redo-in-region fragment)
	    (push fragment (undo-tree-node-next node))
	    (setf (undo-tree-node-branch node) 0
		  (undo-tree-node-previous fragment) node)
	    nil)  ; return nil to indicate failure

	;; otherwise, add redo-in-region node to top of fragment, and attach
	;; it below current node
	(setq fragment
	      (if fragment
		  (undo-tree-grow-backwards fragment nil region-changeset)
		(undo-tree-make-node nil nil region-changeset)))
	(push fragment (undo-tree-node-next node))
	(setf (undo-tree-node-branch node) 0
	      (undo-tree-node-previous fragment) node)
	;; update undo-tree size
	(unless repeated-redo-in-region
	  (setq node fragment)
	  (while (progn
		   (and (setq node (car (undo-tree-node-next node)))
			(incf (undo-tree-size buffer-undo-tree)
			      (undo-list-byte-size
			       (undo-tree-node-redo node)))))))
	(incf (undo-tree-size buffer-undo-tree)
	      (undo-list-byte-size (undo-tree-node-redo fragment)))
	t)  ; indicate undo-in-region branch was successfully pulled
      )))



(defun undo-tree-adjust-elements-to-elt (node undo-elt &optional below)
  "Adjust buffer positions of undo elements, starting at NODE's
and going up the tree (or down the active branch if BELOW is
non-nil) and through the nodes' undo elements until we reach
UNDO-ELT.  UNDO-ELT must appear somewhere in the undo changeset
of either NODE itself or some node above it in the tree."
  (let ((delta (list (undo-delta undo-elt)))
	(undo-list (undo-tree-node-undo node)))
    ;; adjust elements until we reach UNDO-ELT
    (while (and (car undo-list)
		(not (eq (car undo-list) undo-elt)))
      (setcar undo-list
	      (undo-tree-apply-deltas (car undo-list) delta -1))
      ;; move to next undo element in list, or to next node if we've run out
      ;; of elements
      (unless (car (setq undo-list (cdr undo-list)))
	(if below
	    (setq node (nth (undo-tree-node-branch node)
			    (undo-tree-node-next node)))
	  (setq node (undo-tree-node-previous node)))
	(setq undo-list (undo-tree-node-undo node))))))



(defun undo-tree-apply-deltas (undo-elt deltas &optional sgn)
  ;; Apply DELTAS in order to UNDO-ELT, multiplying deltas by SGN
  ;; (only useful value for SGN is -1).
  (let (position offset)
    (dolist (delta deltas)
      (setq position (car delta)
	    offset (* (cdr delta) (or sgn 1)))
      (cond
       ;; POSITION
       ((integerp undo-elt)
	(when (>= undo-elt position)
	  (setq undo-elt (- undo-elt offset))))
       ;; nil (or any other atom)
       ((atom undo-elt))
       ;; (TEXT . POSITION)
       ((stringp (car undo-elt))
	(let ((text-pos (abs (cdr undo-elt)))
	      (point-at-end (< (cdr undo-elt) 0)))
	  (if (>= text-pos position)
	      (setcdr undo-elt (* (if point-at-end -1 1)
				  (- text-pos offset))))))
       ;; (BEGIN . END)
       ((integerp (car undo-elt))
	(when (>= (car undo-elt) position)
	  (setcar undo-elt (- (car undo-elt) offset))
	  (setcdr undo-elt (- (cdr undo-elt) offset))))
       ;; (nil PROPERTY VALUE BEG . END)
       ((null (car undo-elt))
	(let ((tail (nthcdr 3 undo-elt)))
	  (when (>= (car tail) position)
	    (setcar tail (- (car tail) offset))
	    (setcdr tail (- (cdr tail) offset)))))
       ))
    undo-elt))



(defun undo-tree-repeated-undo-in-region-p (start end)
  ;; Return non-nil if undo-in-region between START and END is a repeated
  ;; undo-in-region
  (let ((node (undo-tree-current buffer-undo-tree)))
    (and (setq node
	       (nth (undo-tree-node-branch node) (undo-tree-node-next node)))
	 (eq (undo-tree-node-undo-beginning node) start)
	 (eq (undo-tree-node-undo-end node) end))))


(defun undo-tree-repeated-redo-in-region-p (start end)
  ;; Return non-nil if undo-in-region between START and END is a repeated
  ;; undo-in-region
  (let ((node (undo-tree-current buffer-undo-tree)))
    (and (eq (undo-tree-node-redo-beginning node) start)
	 (eq (undo-tree-node-redo-end node) end))))


;; Return non-nil if undo-in-region between START and END is simply
;; reverting the last redo-in-region
(defalias 'undo-tree-reverting-undo-in-region-p
  'undo-tree-repeated-undo-in-region-p)


;; Return non-nil if redo-in-region between START and END is simply
;; reverting the last undo-in-region
(defalias 'undo-tree-reverting-redo-in-region-p
  'undo-tree-repeated-redo-in-region-p)




;;; =====================================================================
;;;                        Undo-tree commands

;;;###autoload
(define-minor-mode undo-tree-mode
  "Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-map}"

  nil                       ; init value
  undo-tree-mode-lighter    ; lighter
  undo-tree-map             ; keymap

  (cond
   ;; if enabling `undo-tree-mode', set up history-saving hooks if
   ;; `undo-tree-auto-save-history' is enabled
   (undo-tree-mode
    (when undo-tree-auto-save-history
      (add-hook 'write-file-functions 'undo-tree-save-history-hook nil t)
      (add-hook 'find-file-hook 'undo-tree-load-history-hook nil t)))
   ;; if disabling `undo-tree-mode', rebuild `buffer-undo-list' from tree so
   ;; Emacs undo can work
   (t
    (undo-list-rebuild-from-tree)
    (setq buffer-undo-tree nil)
    (when undo-tree-auto-save-history
      (remove-hook 'write-file-functions 'undo-tree-save-history-hook t)
      (remove-hook 'find-file-hook 'undo-tree-load-history-hook t)))))


(defun turn-on-undo-tree-mode (&optional print-message)
  "Enable `undo-tree-mode' in the current buffer, when appropriate.
Some major modes implement their own undo system, which should
not normally be overridden by `undo-tree-mode'. This command does
not enable `undo-tree-mode' in such buffers. If you want to force
`undo-tree-mode' to be enabled regardless, use (undo-tree-mode 1)
instead.

The heuristic used to detect major modes in which
`undo-tree-mode' should not be used is to check whether either
the `undo' command has been remapped, or the default undo
keybindings (C-/ and C-_) have been overridden somewhere other
than in the global map. In addition, `undo-tree-mode' will not be
enabled if the buffer's `major-mode' appears in
`undo-tree-incompatible-major-modes'."
  (interactive "p")
  (if (or (key-binding [remap undo])
	  (undo-tree-overridden-undo-bindings-p)
	  (memq major-mode undo-tree-incompatible-major-modes))
      (when print-message
	(message "Buffer does not support undo-tree-mode;\
 undo-tree-mode NOT enabled"))
    (undo-tree-mode 1)))


(defun undo-tree-overridden-undo-bindings-p ()
  "Returns t if default undo bindings are overridden, nil otherwise.
Checks if either of the default undo key bindings (\"C-/\" or
\"C-_\") are overridden in the current buffer by any keymap other
than the global one. (So global redefinitions of the default undo
key bindings do not count.)"
  (let ((binding1 (lookup-key (current-global-map) [?\C-/]))
	(binding2 (lookup-key (current-global-map) [?\C-_])))
    (global-set-key [?\C-/] 'undo)
    (global-set-key [?\C-_] 'undo)
    (unwind-protect
	(or (and (key-binding [?\C-/])
		 (not (eq (key-binding [?\C-/]) 'undo)))
	    (and (key-binding [?\C-_])
		 (not (eq (key-binding [?\C-_]) 'undo))))
      (global-set-key [?\C-/] binding1)
      (global-set-key [?\C-_] binding2))))


;;;###autoload
(define-globalized-minor-mode global-undo-tree-mode
  undo-tree-mode turn-on-undo-tree-mode)



(defun undo-tree-undo (&optional arg)
  "Undo changes.
Repeat this command to undo more changes.
A numeric ARG serves as a repeat count.

In Transient Mark mode when the mark is active, only undo changes
within the current region. Similarly, when not in Transient Mark
mode, just \\[universal-argument] as an argument limits undo to
changes within the current region."
  (interactive "*P")
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t) (error "No undo information in this buffer"))
  (undo-tree-undo-1 arg)
  ;; inform user if at branch point
  (when (> (undo-tree-num-branches) 1) (message "Undo branch point!")))


(defun undo-tree-undo-1 (&optional arg preserve-redo preserve-timestamps)
  ;; Internal undo function. An active mark in `transient-mark-mode', or
  ;; non-nil ARG otherwise, enables undo-in-region. Non-nil PRESERVE-REDO
  ;; causes the existing redo record to be preserved, rather than replacing it
  ;; with the new one generated by undoing. Non-nil PRESERVE-TIMESTAMPS
  ;; disables updating of timestamps in visited undo-tree nodes. (This latter
  ;; should *only* be used when temporarily visiting another undo state and
  ;; immediately returning to the original state afterwards. Otherwise, it
  ;; could cause history-discarding errors.)
  (let ((undo-in-progress t)
	(undo-in-region (and undo-tree-enable-undo-in-region
			     (or (region-active-p)
				 (and arg (not (numberp arg))))))
	pos current)
    ;; transfer entries accumulated in `buffer-undo-list' to
    ;; `buffer-undo-tree'
    (undo-list-transfer-to-tree)

    (dotimes (i (or (and (numberp arg) (prefix-numeric-value arg)) 1))
      ;; check if at top of undo tree
      (unless (undo-tree-node-previous (undo-tree-current buffer-undo-tree))
	(error "No further undo information"))

      ;; if region is active, or a non-numeric prefix argument was supplied,
      ;; try to pull out a new branch of changes affecting the region
      (when (and undo-in-region
		 (not (undo-tree-pull-undo-in-region-branch
		       (region-beginning) (region-end))))
	(error "No further undo information for region"))

      ;; remove any GC'd elements from node's undo list
      (setq current (undo-tree-current buffer-undo-tree))
      (decf (undo-tree-size buffer-undo-tree)
	    (undo-list-byte-size (undo-tree-node-undo current)))
      (setf (undo-tree-node-undo current)
	    (undo-list-clean-GCd-elts (undo-tree-node-undo current)))
      (incf (undo-tree-size buffer-undo-tree)
	    (undo-list-byte-size (undo-tree-node-undo current)))
      ;; undo one record from undo tree
      (when undo-in-region
	(setq pos (set-marker (make-marker) (point)))
	(set-marker-insertion-type pos t))
      (primitive-undo 1 (undo-tree-copy-list (undo-tree-node-undo current)))
      (undo-boundary)

      ;; if preserving old redo record, discard new redo entries that
      ;; `primitive-undo' has added to `buffer-undo-list', and remove any GC'd
      ;; elements from node's redo list
      (if preserve-redo
	  (progn
	    (undo-list-pop-changeset)
	    (decf (undo-tree-size buffer-undo-tree)
		  (undo-list-byte-size (undo-tree-node-redo current)))
	    (setf (undo-tree-node-redo current)
		  (undo-list-clean-GCd-elts (undo-tree-node-redo current)))
	    (incf (undo-tree-size buffer-undo-tree)
		  (undo-list-byte-size (undo-tree-node-redo current))))
	;; otherwise, record redo entries that `primitive-undo' has added to
	;; `buffer-undo-list' in current node's redo record, replacing
	;; existing entry if one already exists
	(when (undo-tree-node-redo current)
	  (decf (undo-tree-size buffer-undo-tree)
		(undo-list-byte-size (undo-tree-node-redo current))))
	(setf (undo-tree-node-redo current)
	      (undo-list-pop-changeset 'discard-pos))
	(incf (undo-tree-size buffer-undo-tree)
	      (undo-list-byte-size (undo-tree-node-redo current))))

      ;; rewind current node and update timestamp
      (setf (undo-tree-current buffer-undo-tree)
	    (undo-tree-node-previous (undo-tree-current buffer-undo-tree)))
      (unless preserve-timestamps
	(setf (undo-tree-node-timestamp (undo-tree-current buffer-undo-tree))
	      (current-time)))

      ;; if undoing-in-region, record current node, region and direction so we
      ;; can tell if undo-in-region is repeated, and re-activate mark if in
      ;; `transient-mark-mode'; if not, erase any leftover data
      (if (not undo-in-region)
	  (undo-tree-node-clear-region-data current)
	(goto-char pos)
	;; note: we deliberately want to store the region information in the
	;; node *below* the now current one
	(setf (undo-tree-node-undo-beginning current) (region-beginning)
	      (undo-tree-node-undo-end current) (region-end))
	(set-marker pos nil)))

    ;; undo deactivates mark unless undoing-in-region
    (setq deactivate-mark (not undo-in-region))))



(defun undo-tree-redo (&optional arg)
  "Redo changes. A numeric ARG serves as a repeat count.

In Transient Mark mode when the mark is active, only redo changes
within the current region. Similarly, when not in Transient Mark
mode, just \\[universal-argument] as an argument limits redo to
changes within the current region."
  (interactive "*P")
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t) (error "No undo information in this buffer"))
  (undo-tree-redo-1 arg)
  ;; inform user if at branch point
  (when (> (undo-tree-num-branches) 1) (message "Undo branch point!")))


(defun undo-tree-redo-1 (&optional arg preserve-undo preserve-timestamps)
  ;; Internal redo function. An active mark in `transient-mark-mode', or
  ;; non-nil ARG otherwise, enables undo-in-region. Non-nil PRESERVE-UNDO
  ;; causes the existing redo record to be preserved, rather than replacing it
  ;; with the new one generated by undoing. Non-nil PRESERVE-TIMESTAMPS
  ;; disables updating of timestamps in visited undo-tree nodes. (This latter
  ;; should *only* be used when temporarily visiting another undo state and
  ;; immediately returning to the original state afterwards. Otherwise, it
  ;; could cause history-discarding errors.)
  (let ((undo-in-progress t)
	(redo-in-region (and undo-tree-enable-undo-in-region
			     (or (region-active-p)
				 (and arg (not (numberp arg))))))
	pos current)
    ;; transfer entries accumulated in `buffer-undo-list' to
    ;; `buffer-undo-tree'
    (undo-list-transfer-to-tree)

    (dotimes (i (or (and (numberp arg) (prefix-numeric-value arg)) 1))
      ;; check if at bottom of undo tree
      (when (null (undo-tree-node-next (undo-tree-current buffer-undo-tree)))
	(error "No further redo information"))

      ;; if region is active, or a non-numeric prefix argument was supplied,
      ;; try to pull out a new branch of changes affecting the region
      (when (and redo-in-region
		 (not (undo-tree-pull-redo-in-region-branch
		       (region-beginning) (region-end))))
	(error "No further redo information for region"))

      ;; advance current node
      (setq current (undo-tree-current buffer-undo-tree)
	    current (setf (undo-tree-current buffer-undo-tree)
			  (nth (undo-tree-node-branch current)
			       (undo-tree-node-next current))))
      ;; remove any GC'd elements from node's redo list
      (decf (undo-tree-size buffer-undo-tree)
	    (undo-list-byte-size (undo-tree-node-redo current)))
      (setf (undo-tree-node-redo current)
	    (undo-list-clean-GCd-elts (undo-tree-node-redo current)))
      (incf (undo-tree-size buffer-undo-tree)
	    (undo-list-byte-size (undo-tree-node-redo current)))
      ;; redo one record from undo tree
      (when redo-in-region
	(setq pos (set-marker (make-marker) (point)))
	(set-marker-insertion-type pos t))
      (primitive-undo 1 (undo-tree-copy-list (undo-tree-node-redo current)))
      (undo-boundary)

      ;; if preserving old undo record, discard new undo entries that
      ;; `primitive-undo' has added to `buffer-undo-list', and remove any GC'd
      ;; elements from node's redo list
      (if preserve-undo
	  (progn
	    (undo-list-pop-changeset)
	    (decf (undo-tree-size buffer-undo-tree)
		  (undo-list-byte-size (undo-tree-node-undo current)))
	    (setf (undo-tree-node-undo current)
		  (undo-list-clean-GCd-elts (undo-tree-node-undo current)))
	    (incf (undo-tree-size buffer-undo-tree)
		  (undo-list-byte-size (undo-tree-node-undo current))))
	;; otherwise, record undo entries that `primitive-undo' has added to
	;; `buffer-undo-list' in current node's undo record, replacing
	;; existing entry if one already exists
	(when (undo-tree-node-undo current)
	  (decf (undo-tree-size buffer-undo-tree)
		(undo-list-byte-size (undo-tree-node-undo current))))
	(setf (undo-tree-node-undo current)
	      (undo-list-pop-changeset 'discard-pos))
	(incf (undo-tree-size buffer-undo-tree)
	      (undo-list-byte-size (undo-tree-node-undo current))))

      ;; update timestamp
      (unless preserve-timestamps
	(setf (undo-tree-node-timestamp current) (current-time)))

      ;; if redoing-in-region, record current node, region and direction so we
      ;; can tell if redo-in-region is repeated, and re-activate mark if in
      ;; `transient-mark-mode'
      (if (not redo-in-region)
	  (undo-tree-node-clear-region-data current)
	(goto-char pos)
	(setf (undo-tree-node-redo-beginning current) (region-beginning)
	      (undo-tree-node-redo-end current) (region-end))
	(set-marker pos nil)))

    ;; redo deactivates the mark unless redoing-in-region
    (setq deactivate-mark (not redo-in-region))))



(defun undo-tree-switch-branch (branch)
  "Switch to a different BRANCH of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo'."
  (interactive (list (or (and prefix-arg (prefix-numeric-value prefix-arg))
                         (and (not (eq buffer-undo-list t))
			      (or (undo-list-transfer-to-tree) t)
			      (let ((b (undo-tree-node-branch
					(undo-tree-current
					 buffer-undo-tree))))
				(cond
				 ;; switch to other branch if only 2
				 ((= (undo-tree-num-branches) 2) (- 1 b))
				 ;; prompt if more than 2
				 ((> (undo-tree-num-branches) 2)
				  (read-number
				   (format "Branch (0-%d, on %d): "
					   (1- (undo-tree-num-branches)) b)))
				 ))))))
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t) (error "No undo information in this buffer"))
  ;; sanity check branch number
  (when (<= (undo-tree-num-branches) 1) (error "Not at undo branch point"))
  (when (or (< branch 0) (> branch (1- (undo-tree-num-branches))))
    (error "Invalid branch number"))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)
  ;; switch branch
  (setf (undo-tree-node-branch (undo-tree-current buffer-undo-tree))
	branch)
  (message "Switched to branch %d" branch))


(defun undo-tree-set (node &optional preserve-timestamps)
  ;; Set buffer to state corresponding to NODE. Returns intersection point
  ;; between path back from current node and path back from selected NODE.
  ;; Non-nil PRESERVE-TIMESTAMPS disables updating of timestamps in visited
  ;; undo-tree nodes. (This should *only* be used when temporarily visiting
  ;; another undo state and immediately returning to the original state
  ;; afterwards. Otherwise, it could cause history-discarding errors.)
  (let ((path (make-hash-table :test 'eq))
        (n node))
    (puthash (undo-tree-root buffer-undo-tree) t path)
    ;; build list of nodes leading back from selected node to root, updating
    ;; branches as we go to point down to selected node
    (while (progn
             (puthash n t path)
             (when (undo-tree-node-previous n)
               (setf (undo-tree-node-branch (undo-tree-node-previous n))
                     (undo-tree-position
                      n (undo-tree-node-next (undo-tree-node-previous n))))
               (setq n (undo-tree-node-previous n)))))
    ;; work backwards from current node until we intersect path back from
    ;; selected node
    (setq n (undo-tree-current buffer-undo-tree))
    (while (not (gethash n path))
      (setq n (undo-tree-node-previous n)))
    ;; ascend tree until intersection node
    (while (not (eq (undo-tree-current buffer-undo-tree) n))
      (undo-tree-undo-1))
    ;; descend tree until selected node
    (while (not (eq (undo-tree-current buffer-undo-tree) node))
      (undo-tree-redo-1))
    n))  ; return intersection node



(defun undo-tree-save-state-to-register (register)
  "Store current undo-tree state to REGISTER.
The saved state can be restored using
`undo-tree-restore-state-from-register'.
Argument is a character, naming the register."
  (interactive "cUndo-tree state to register: ")
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t) (error "No undo information in this buffer"))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)
  ;; save current node to REGISTER
  (set-register
   register (registerv-make
	     (undo-tree-make-register-data
	      (current-buffer) (undo-tree-current buffer-undo-tree))
	     :print-func 'undo-tree-register-data-print-func))
  ;; record REGISTER in current node, for visualizer
  (setf (undo-tree-node-register (undo-tree-current buffer-undo-tree))
	register))



(defun undo-tree-restore-state-from-register (register)
  "Restore undo-tree state from REGISTER.
The state must be saved using `undo-tree-save-state-to-register'.
Argument is a character, naming the register."
  (interactive "*cRestore undo-tree state from register: ")
  ;; throw error if undo is disabled in buffer, or if register doesn't contain
  ;; an undo-tree node
  (let ((data (registerv-data (get-register register))))
    (cond
     ((eq buffer-undo-list t)
      (error "No undo information in this buffer"))
     ((not (undo-tree-register-data-p data))
      (error "Register doesn't contain undo-tree state"))
     ((not (eq (current-buffer) (undo-tree-register-data-buffer data)))
      (error "Register contains undo-tree state for a different buffer")))
    ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
    (undo-list-transfer-to-tree)
    ;; restore buffer state corresponding to saved node
    (undo-tree-set (undo-tree-register-data-node data))))



(defun undo-tree-make-history-save-file-name ()
  (concat (file-name-directory (buffer-file-name))
	  "." (file-name-nondirectory (buffer-file-name)) ".~undo-tree"))


(defun undo-tree-save-history (&optional filename overwrite)
  "Store undo-tree history to file.

If optional argument FILENAME is omitted, default save file is
\".<buffer-file-name>.~undo-tree\" if buffer is visiting a file.
Otherwise, prompt for one.

If OVERWRITE is non-nil, any existing file will be overwritten
without asking for confirmation."
  (interactive)
  (condition-case nil
      (undo-tree-kill-visualizer)
    (error (undo-tree-clear-visualizer-data buffer-undo-tree)))
  (undo-list-transfer-to-tree)
  (let ((buff (current-buffer))
	(tree (copy-undo-tree buffer-undo-tree)))
    ;; get filename
    (unless filename
      (setq filename
	    (if buffer-file-name
		(undo-tree-make-history-save-file-name)
	      (expand-file-name (read-file-name "File to save in: ") nil))))
    (when (or (not (file-exists-p filename))
	      overwrite
	      (yes-or-no-p (format "Overwrite \"%s\"? " filename)))
      ;; discard undo-tree object pool before saving
      (setf (undo-tree-object-pool tree) nil)
      ;; print undo-tree to file
      (with-temp-file filename
	(prin1 (sha1 buff) (current-buffer))
	(terpri (current-buffer))
	(let ((print-circle t)) (prin1 tree (current-buffer)))))))



(defun undo-tree-load-history (&optional filename noerror)
  "Load undo-tree history from file.

If optional argument FILENAME is null, default load file is
\".<buffer-file-name>.~undo-tree\" if buffer is visiting a file.
Otherwise, prompt for one.

If optional argument NOERROR is non-nil, return nil instead of
signaling an error if file is not found."
  (interactive)
  ;; get filename
  (unless filename
    (setq filename
	  (if buffer-file-name
	      (undo-tree-make-history-save-file-name)
	    (expand-file-name (read-file-name "File to load from: ") nil))))

  ;; attempt to read undo-tree from FILENAME
  (catch 'load-error
    (unless (file-exists-p filename)
      (if noerror
	  (throw 'load-error nil)
	(error "File \"%s\" does not exist; could not load undo-tree history"
	       filename)))
    (let (buff tmp hash tree)
      (setq buff (current-buffer))
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(condition-case nil
	    (setq hash (read (current-buffer)))
	  (error
	   (kill-buffer nil)
	   (funcall (if noerror 'message 'error)
		    "Error reading undo-tree history from \"%s\"" filename)
	   (throw 'load-error nil)))
	(unless (string= (sha1 buff) hash)
	  (kill-buffer nil)
	  (funcall (if noerror 'message 'error)
		   "Buffer has been modified; could not load undo-tree history")
	  (throw 'load-error nil))
	(condition-case nil
	    (setq tree (read (current-buffer)))
	  (error
	   (kill-buffer nil)
	   (funcall (if noerror 'message 'error)
		    "Error reading undo-tree history from \"%s\"" filename)
	   (throw 'load-error nil)))
	(kill-buffer nil))
      ;; initialise empty undo-tree object pool
      (setf (undo-tree-object-pool tree)
	    (make-hash-table :test 'eq :weakness 'value))
      (setq buffer-undo-tree tree))))



;; Versions of save/load functions for use in hooks
(defun undo-tree-save-history-hook ()
  (undo-tree-save-history nil t) nil)

(defun undo-tree-load-history-hook ()
  (undo-tree-load-history nil t))




;;; =====================================================================
;;;                       Undo-tree visualizer

(defun undo-tree-visualize ()
  "Visualize the current buffer's undo tree."
  (interactive "*")
  (deactivate-mark)
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t) (error "No undo information in this buffer"))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)
  ;; add hook to kill visualizer buffer if original buffer is changed
  (add-hook 'before-change-functions 'undo-tree-kill-visualizer nil t)
  ;; prepare *undo-tree* buffer, then draw tree in it
  (let ((undo-tree buffer-undo-tree)
        (buff (current-buffer))
	(display-buffer-mark-dedicated 'soft))
    (switch-to-buffer-other-window
     (get-buffer-create undo-tree-visualizer-buffer-name))
    (setq undo-tree-visualizer-parent-buffer buff)
    (setq buffer-undo-tree undo-tree)
    (setq undo-tree-visualizer-initial-node (undo-tree-current undo-tree))
    (setq undo-tree-visualizer-spacing
	  (undo-tree-visualizer-calculate-spacing))
    (when undo-tree-visualizer-diff (undo-tree-visualizer-show-diff))
    (undo-tree-visualizer-mode)
    (let ((inhibit-read-only t)) (undo-tree-draw-tree undo-tree))))


(defun undo-tree-kill-visualizer (&rest dummy)
  ;; Kill visualizer. Added to `before-change-functions' hook of original
  ;; buffer when visualizer is invoked.
  (unless undo-tree-inhibit-kill-visualizer
    (unwind-protect
	(with-current-buffer undo-tree-visualizer-buffer-name
	  (undo-tree-visualizer-quit)))))



(defun undo-tree-draw-tree (undo-tree)
  ;; Draw UNDO-TREE in current buffer.
  (erase-buffer)
  (undo-tree-move-down 1)      ; top margin
  (undo-tree-clear-visualizer-data undo-tree)
  (undo-tree-compute-widths undo-tree)
  (undo-tree-move-forward
   (max (/ (window-width) 2)
        (+ (undo-tree-node-char-lwidth (undo-tree-root undo-tree))
           ;; add space for left part of left-most time-stamp
           (if undo-tree-visualizer-timestamps
	       (/ (- undo-tree-visualizer-spacing 4) 2)
	     0)
           2)))  ; left margin
  ;; draw undo-tree
  (let ((undo-tree-insert-face 'undo-tree-visualizer-default-face)
        (stack (list (undo-tree-root undo-tree)))
        (n (undo-tree-root undo-tree)))
    ;; link root node to its representation in visualizer
    (unless (markerp (undo-tree-node-marker n))
      (setf (undo-tree-node-marker n) (make-marker))
      (set-marker-insertion-type (undo-tree-node-marker n) nil))
    (move-marker (undo-tree-node-marker n) (point))
    ;; draw nodes from stack until stack is empty
    (while stack
      (setq n (pop stack))
      (goto-char (undo-tree-node-marker n))
      (setq n (undo-tree-draw-subtree n nil))
      (setq stack (append stack n))))
  ;; highlight active branch
  (goto-char (undo-tree-node-marker (undo-tree-root undo-tree)))
  (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face))
    (undo-tree-highlight-active-branch (undo-tree-root undo-tree)))
  ;; highlight current node
  (undo-tree-draw-node (undo-tree-current undo-tree) 'current))


(defun undo-tree-highlight-active-branch (node)
  ;; Draw highlighted active branch below NODE in current buffer.
  (let ((stack (list node)))
    ;; link node to its representation in visualizer
    (unless (markerp (undo-tree-node-marker node))
      (setf (undo-tree-node-marker node) (make-marker))
      (set-marker-insertion-type (undo-tree-node-marker node) nil))
    (move-marker (undo-tree-node-marker node) (point))
    ;; draw active branch
    (while stack
      (setq node (pop stack))
      (goto-char (undo-tree-node-marker node))
      (setq node (undo-tree-draw-subtree node 'active))
      (setq stack (append stack node)))))


(defun undo-tree-draw-node (node &optional current)
  ;; Draw symbol representing NODE in visualizer.
  (goto-char (undo-tree-node-marker node))
  (when undo-tree-visualizer-timestamps
    (backward-char (/ undo-tree-visualizer-spacing 2)))

  (let ((register (undo-tree-node-register node))
	node-string)
    (unless (and register
		 (eq node (undo-tree-register-data-node
			   (registerv-data (get-register register)))))
      (setq register nil))
    ;; represent node by differentl symbols, depending on whether it's the
    ;; current node or is saved in a register
    (setq node-string
	  (cond
	   (undo-tree-visualizer-timestamps
	    (undo-tree-timestamp-to-string
	     (undo-tree-node-timestamp node)
	     undo-tree-visualizer-relative-timestamps
	     current register))
	   (current "x")
	   (register (char-to-string register))
	   (t "o")))

    (cond
     (current
      (let ((undo-tree-insert-face
             (cons 'undo-tree-visualizer-current-face
                   (and (boundp 'undo-tree-insert-face)
                        (or (and (consp undo-tree-insert-face)
                                 undo-tree-insert-face)
                            (list undo-tree-insert-face))))))
        (undo-tree-insert node-string)))
     (register
      (let ((undo-tree-insert-face
             (cons 'undo-tree-visualizer-register-face
                   (and (boundp 'undo-tree-insert-face)
                        (or (and (consp undo-tree-insert-face)
                                 undo-tree-insert-face)
                            (list undo-tree-insert-face))))))
        (undo-tree-insert node-string)))
     (t (undo-tree-insert node-string)))

    (backward-char (if undo-tree-visualizer-timestamps
		       (1+ (/ undo-tree-visualizer-spacing 2))
		     1))
    (move-marker (undo-tree-node-marker node) (point))
    (put-text-property (point) (1+ (point)) 'undo-tree-node node)))


(defun undo-tree-draw-subtree (node &optional active-branch)
  ;; Draw subtree rooted at NODE. The subtree will start from point.
  ;; If ACTIVE-BRANCH is non-nil, just draw active branch below NODE.
  ;; If TIMESTAP is non-nil, draw time-stamps instead of "o" at nodes.
  (let ((num-children (length (undo-tree-node-next node)))
        node-list pos trunk-pos n)
    ;; draw node itself
    (undo-tree-draw-node node)

    (cond
     ;; if we're at a leaf node, we're done
     ((= num-children 0))

     ;; if node has only one child, draw it (not strictly necessary to deal
     ;; with this case separately, but as it's by far the most common case
     ;; this makes the code clearer and more efficient)
     ((= num-children 1)
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (backward-char 1)
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (backward-char 1)
      (undo-tree-move-down 1)
      (setq n (car (undo-tree-node-next node)))
      ;; link next node to its representation in visualizer
      (unless (markerp (undo-tree-node-marker n))
        (setf (undo-tree-node-marker n) (make-marker))
        (set-marker-insertion-type (undo-tree-node-marker n) nil))
      (move-marker (undo-tree-node-marker n) (point))
      ;; add next node to list of nodes to draw next
      (push n node-list))

     ;; if node had multiple children, draw branches
     (t
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (backward-char 1)
      (setq trunk-pos (point))
      ;; left subtrees
      (backward-char
       (- (undo-tree-node-char-lwidth node)
          (undo-tree-node-char-lwidth
           (car (undo-tree-node-next node)))))
      (setq pos (point))
      (setq n (cons nil (undo-tree-node-next node)))
      (dotimes (i (/ num-children 2))
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (undo-tree-node-branch node)
                           (undo-tree-node-next node))))
          (undo-tree-move-forward 2)
          (undo-tree-insert ?_ (- trunk-pos pos 2))
          (goto-char pos)
          (undo-tree-move-forward 1)
          (undo-tree-move-down 1)
          (undo-tree-insert ?/)
          (backward-char 2)
          (undo-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (undo-tree-node-marker (car n)))
            (setf (undo-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (undo-tree-node-marker (car n)) nil))
          (move-marker (undo-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (goto-char pos)
        (undo-tree-move-forward
         (+ (undo-tree-node-char-rwidth (car n))
            (undo-tree-node-char-lwidth (cadr n))
            undo-tree-visualizer-spacing 1))
        (setq pos (point)))
      ;; middle subtree (only when number of children is odd)
      (when (= (mod num-children 2) 1)
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (undo-tree-node-branch node)
                           (undo-tree-node-next node))))
          (undo-tree-move-down 1)
          (undo-tree-insert ?|)
          (backward-char 1)
          (undo-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (undo-tree-node-marker (car n)))
            (setf (undo-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (undo-tree-node-marker (car n)) nil))
          (move-marker (undo-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (goto-char pos)
        (undo-tree-move-forward
         (+ (undo-tree-node-char-rwidth (car n))
            (if (cadr n) (undo-tree-node-char-lwidth (cadr n)) 0)
            undo-tree-visualizer-spacing 1))
        (setq pos (point)))
      ;; right subtrees
      (incf trunk-pos)
      (dotimes (i (/ num-children 2))
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (undo-tree-node-branch node)
                           (undo-tree-node-next node))))
          (goto-char trunk-pos)
          (undo-tree-insert ?_ (- pos trunk-pos 1))
          (goto-char pos)
          (backward-char 1)
          (undo-tree-move-down 1)
          (undo-tree-insert ?\\)
          (undo-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (undo-tree-node-marker (car n)))
            (setf (undo-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (undo-tree-node-marker (car n)) nil))
          (move-marker (undo-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (when (cdr n)
          (goto-char pos)
          (undo-tree-move-forward
           (+ (undo-tree-node-char-rwidth (car n))
              (if (cadr n) (undo-tree-node-char-lwidth (cadr n)) 0)
              undo-tree-visualizer-spacing 1))
          (setq pos (point))))
      ))
    ;; return list of nodes to draw next
    (nreverse node-list)))



(defun undo-tree-node-char-lwidth (node)
  ;; Return left-width of NODE measured in characters.
  (if (= (length (undo-tree-node-next node)) 0) 0
    (- (* (+ undo-tree-visualizer-spacing 1) (undo-tree-node-lwidth node))
       (if (= (undo-tree-node-cwidth node) 0)
           (1+ (/ undo-tree-visualizer-spacing 2)) 0))))


(defun undo-tree-node-char-rwidth (node)
  ;; Return right-width of NODE measured in characters.
  (if (= (length (undo-tree-node-next node)) 0) 0
    (- (* (+ undo-tree-visualizer-spacing 1) (undo-tree-node-rwidth node))
       (if (= (undo-tree-node-cwidth node) 0)
           (1+ (/ undo-tree-visualizer-spacing 2)) 0))))


(defun undo-tree-insert (str &optional arg)
  ;; Insert character or string STR ARG times, overwriting, and using
  ;; `undo-tree-insert-face'.
  (unless arg (setq arg 1))
  (when (characterp str)
    (setq str (make-string arg str))
    (setq arg 1))
  (dotimes (i arg) (insert str))
  (setq arg (* arg (length str)))
  (undo-tree-move-forward arg)
  ;; make sure mark isn't active, otherwise `backward-delete-char' might
  ;; delete region instead of single char if transient-mark-mode is enabled
  (setq mark-active nil)
  (backward-delete-char arg)
  (when (boundp 'undo-tree-insert-face)
    (put-text-property (- (point) arg) (point) 'face undo-tree-insert-face)))


(defun undo-tree-move-down (&optional arg)
  ;; Move down, extending buffer if necessary.
  (let ((row (line-number-at-pos))
        (col (current-column))
        line)
    (unless arg (setq arg 1))
    (forward-line arg)
    (setq line (line-number-at-pos))
    ;; if buffer doesn't have enough lines, add some
    (when (/= line (+ row arg))
      (insert (make-string (- arg (- line row)) ?\n)))
    (undo-tree-move-forward col)))


(defun undo-tree-move-forward (&optional arg)
  ;; Move forward, extending buffer if necessary.
  (unless arg (setq arg 1))
  (let ((n (- (line-end-position) (point))))
    (if (> n arg)
        (forward-char arg)
      (end-of-line)
      (insert (make-string (- arg n) ? )))))


(defun undo-tree-timestamp-to-string
  (timestamp &optional relative current register)
  ;; Convert TIMESTAMP to string (either absolute or RELATVE time), indicating
  ;; if it's the CURRENT node and/or has an associated REGISTER.
  (if relative
      ;; relative time
      (let ((time (floor (float-time
			  (subtract-time (current-time) timestamp))))
	    n)
	(setq time
	      ;; years
	      (if (> (setq n (/ time 315360000)) 0)
		  (if (> n 999) "-ages" (format "-%dy" n))
		(setq time (% time 315360000))
		;; days
		(if (> (setq n (/ time 86400)) 0)
		    (format "-%dd" n)
		  (setq time (% time 86400))
		  ;; hours
		  (if (> (setq n (/ time 3600)) 0)
		      (format "-%dh" n)
		    (setq time (% time 3600))
		    ;; mins
		    (if (> (setq n (/ time 60)) 0)
			(format "-%dm" n)
		      ;; secs
		      (format "-%ds" (% time 60)))))))
	(setq time (concat
		    (if current "*" " ")
		    time
		    (if register (concat "[" (char-to-string register) "]")
		      "   ")))
	(setq n (length time))
	(if (< n 9)
	    (concat (make-string (- 9 n) ? ) time)
	  time))
    ;; absolute time
    (concat (if current "*" " ")
	    (format-time-string "%H:%M:%S" timestamp)
	    (if register
		(concat "[" (char-to-string register) "]")
	      "   "))))




;;; =====================================================================
;;;                    Visualizer mode commands

(defun undo-tree-visualizer-mode ()
  "Major mode used in undo-tree visualizer.

The undo-tree visualizer can only be invoked from a buffer in
which `undo-tree-mode' is enabled. The visualizer displays the
undo history tree graphically, and allows you to browse around
the undo history, undoing or redoing the corresponding changes in
the parent buffer.

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-map}"
  (interactive)
  (setq major-mode 'undo-tree-visualizer-mode)
  (setq mode-name "undo-tree-visualizer-mode")
  (use-local-map undo-tree-visualizer-map)
  (setq truncate-lines t)
  (setq cursor-type nil)
  (setq buffer-read-only t)
  (setq undo-tree-visualizer-selected-node nil)
  (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff)))



(defun undo-tree-visualize-undo (&optional arg)
  "Undo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face)
	(inhibit-read-only t))
    (undo-tree-draw-node (undo-tree-current buffer-undo-tree)))
  (switch-to-buffer-other-window undo-tree-visualizer-parent-buffer)
  (deactivate-mark)
  (unwind-protect
      (let ((undo-tree-inhibit-kill-visualizer t)) (undo-tree-undo arg))
    (switch-to-buffer-other-window undo-tree-visualizer-buffer-name)
    (let ((inhibit-read-only t))
      (undo-tree-draw-node (undo-tree-current buffer-undo-tree) 'current))
    (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff))))


(defun undo-tree-visualize-redo (&optional arg)
  "Redo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face)
	(inhibit-read-only t))
    (undo-tree-draw-node (undo-tree-current buffer-undo-tree)))
  (switch-to-buffer-other-window undo-tree-visualizer-parent-buffer)
  (deactivate-mark)
  (unwind-protect
      (let ((undo-tree-inhibit-kill-visualizer t)) (undo-tree-redo arg))
    (switch-to-buffer-other-window undo-tree-visualizer-buffer-name)
    (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
    (let ((inhibit-read-only t))
      (undo-tree-draw-node (undo-tree-current buffer-undo-tree) 'current))
    (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff))))


(defun undo-tree-visualize-switch-branch-right (arg)
  "Switch to next branch of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo' or `undo-tree-visualizer-redo'."
  (interactive "p")
  ;; un-highlight old active branch below current node
  (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
  (let ((undo-tree-insert-face 'undo-tree-visualizer-default-face)
	(inhibit-read-only t))
    (undo-tree-highlight-active-branch (undo-tree-current buffer-undo-tree)))
  ;; increment branch
  (let ((branch (undo-tree-node-branch (undo-tree-current buffer-undo-tree))))
  (setf (undo-tree-node-branch (undo-tree-current buffer-undo-tree))
        (cond
         ((>= (+ branch arg) (undo-tree-num-branches))
          (1- (undo-tree-num-branches)))
         ((<= (+ branch arg) 0) 0)
         (t (+ branch arg))))
  (let ((inhibit-read-only t))
    ;; highlight new active branch below current node
    (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
    (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face))
      (undo-tree-highlight-active-branch (undo-tree-current buffer-undo-tree)))
    ;; re-highlight current node
    (undo-tree-draw-node (undo-tree-current buffer-undo-tree) 'current))))


(defun undo-tree-visualize-switch-branch-left (arg)
  "Switch to previous branch of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo' or `undo-tree-visualizer-redo'."
  (interactive "p")
  (undo-tree-visualize-switch-branch-right (- arg)))


(defun undo-tree-visualizer-quit ()
  "Quit the undo-tree visualizer."
  (interactive)
  (undo-tree-clear-visualizer-data buffer-undo-tree)
  ;; remove kill visualizer hook from parent buffer
  (unwind-protect
      (with-current-buffer undo-tree-visualizer-parent-buffer
	(remove-hook 'before-change-functions 'undo-tree-kill-visualizer t))
    ;; kill diff buffer, if any
    (when undo-tree-visualizer-diff (undo-tree-visualizer-hide-diff))
    (let ((parent undo-tree-visualizer-parent-buffer)
	  window)
      ;; kill visualizer buffer
      (kill-buffer nil)
      ;; switch back to parent buffer
      (unwind-protect
	  (if (setq window (get-buffer-window parent))
	      (select-window window)
	    (switch-to-buffer parent))))))


(defun undo-tree-visualizer-abort ()
  "Quit the undo-tree visualizer and return buffer to original state."
  (interactive)
  (let ((node undo-tree-visualizer-initial-node))
    (undo-tree-visualizer-quit)
    (undo-tree-set node)))


(defun undo-tree-visualizer-set (&optional pos)
  "Set buffer to state corresponding to undo tree node
at POS, or point if POS is nil."
  (interactive)
  (unless pos (setq pos (point)))
  (let ((node (get-text-property pos 'undo-tree-node)))
    (when node
      ;; set parent buffer to state corresponding to node at POS
      (switch-to-buffer-other-window undo-tree-visualizer-parent-buffer)
      (let ((undo-tree-inhibit-kill-visualizer t)) (undo-tree-set node))
      (switch-to-buffer-other-window undo-tree-visualizer-buffer-name)
      ;; re-draw undo tree
      (let ((inhibit-read-only t)) (undo-tree-draw-tree buffer-undo-tree))
      (when undo-tree-visualizer-diff (undo-tree-visualizer-update-diff)))))


(defun undo-tree-visualizer-mouse-set (pos)
  "Set buffer to state corresponding to undo tree node
at mouse event POS."
  (interactive "@e")
  (undo-tree-visualizer-set (event-start (nth 1 pos))))


(defun undo-tree-visualizer-toggle-timestamps ()
  "Toggle display of time-stamps."
  (interactive)
  (setq undo-tree-visualizer-timestamps (not undo-tree-visualizer-timestamps))
  (setq undo-tree-visualizer-spacing (undo-tree-visualizer-calculate-spacing))
  ;; redraw tree
  (let ((inhibit-read-only t)) (undo-tree-draw-tree buffer-undo-tree)))


(defun undo-tree-visualizer-scroll-left (&optional arg)
  (interactive "p")
  (scroll-right (or arg 1) t))


(defun undo-tree-visualizer-scroll-right (&optional arg)
  (interactive "p")
  (scroll-left (or arg 1) t))




;;; =====================================================================
;;;                    Visualizer selection mode

(defun undo-tree-visualizer-selection-mode ()
  "Major mode used to select nodes in undo-tree visualizer."
  (interactive)
  (setq major-mode 'undo-tree-visualizer-selection-mode)
  (setq mode-name "undo-tree-visualizer-selection-mode")
  (use-local-map undo-tree-visualizer-selection-map)
  (setq cursor-type 'box)
  (setq undo-tree-visualizer-selected-node
	(undo-tree-current buffer-undo-tree))
  ;; erase diff (if any), as initially selected node is identical to current
  (when undo-tree-visualizer-diff
    (let ((buff (get-buffer undo-tree-diff-buffer-name))
	  (inhibit-read-only t))
      (when buff (with-current-buffer buff (erase-buffer))))))


(defun undo-tree-visualizer-select-previous (&optional arg)
  "Move to previous node."
  (interactive "p")
  (let ((node undo-tree-visualizer-selected-node))
    (catch 'top
      (dotimes (i arg)
	(unless (undo-tree-node-previous node) (throw 'top t))
	(setq node (undo-tree-node-previous node))))
    (goto-char (undo-tree-node-marker node))
    (when (and undo-tree-visualizer-diff
	       (not (eq node undo-tree-visualizer-selected-node)))
      (undo-tree-visualizer-update-diff node))
    (setq undo-tree-visualizer-selected-node node)))


(defun undo-tree-visualizer-select-next (&optional arg)
  "Move to next node."
  (interactive "p")
  (let ((node undo-tree-visualizer-selected-node))
    (catch 'bottom
      (dotimes (i arg)
	(unless (nth (undo-tree-node-branch node) (undo-tree-node-next node))
	  (throw 'bottom t))
	(setq node
	      (nth (undo-tree-node-branch node) (undo-tree-node-next node)))))
    (goto-char (undo-tree-node-marker node))
    (when (and undo-tree-visualizer-diff
	       (not (eq node undo-tree-visualizer-selected-node)))
      (undo-tree-visualizer-update-diff node))
    (setq undo-tree-visualizer-selected-node node)))


(defun undo-tree-visualizer-select-right (&optional arg)
  "Move right to a sibling node."
  (interactive "p")
  (let ((node undo-tree-visualizer-selected-node)
	end)
    (goto-char (undo-tree-node-marker undo-tree-visualizer-selected-node))
    (setq end (line-end-position))
    (catch 'end
      (dotimes (i arg)
	(while (or (null node) (eq node undo-tree-visualizer-selected-node))
	  (forward-char)
	  (setq node (get-text-property (point) 'undo-tree-node))
	  (when (= (point) end) (throw 'end t)))))
    (goto-char (undo-tree-node-marker
		(or node undo-tree-visualizer-selected-node)))
    (when (and undo-tree-visualizer-diff node
	       (not (eq node undo-tree-visualizer-selected-node)))
      (undo-tree-visualizer-update-diff node))
    (setq undo-tree-visualizer-selected-node node)))


(defun undo-tree-visualizer-select-left (&optional arg)
  "Move left to a sibling node."
  (interactive "p")
  (let ((node (get-text-property (point) 'undo-tree-node))
	beg)
    (goto-char (undo-tree-node-marker undo-tree-visualizer-selected-node))
    (setq beg (line-beginning-position))
    (catch 'beg
      (dotimes (i arg)
	(while (or (null node) (eq node undo-tree-visualizer-selected-node))
	  (backward-char)
	  (setq node (get-text-property (point) 'undo-tree-node))
	  (when (= (point) beg) (throw 'beg t)))))
    (goto-char (undo-tree-node-marker
		(or node undo-tree-visualizer-selected-node)))
    (when (and undo-tree-visualizer-diff node
	       (not (eq node undo-tree-visualizer-selected-node)))
      (undo-tree-visualizer-update-diff node))
    (setq undo-tree-visualizer-selected-node node)))



;;; =====================================================================
;;;                      Visualizer diff display

(defun undo-tree-visualizer-toggle-diff ()
  "Toggle diff display in undo-tree visualizer."
  (interactive)
  (if undo-tree-visualizer-diff
      (undo-tree-visualizer-hide-diff)
    (undo-tree-visualizer-show-diff)))


(defun undo-tree-visualizer-selection-toggle-diff ()
  "Toggle diff display in undo-tree visualizer selection mode."
  (interactive)
  (if undo-tree-visualizer-diff
      (undo-tree-visualizer-hide-diff)
    (let ((node (get-text-property (point) 'undo-tree-node)))
      (when node (undo-tree-visualizer-show-diff node)))))


(defun undo-tree-visualizer-show-diff (&optional node)
  ;; show visualizer diff display
  (setq undo-tree-visualizer-diff t)
  (let ((buff (with-current-buffer undo-tree-visualizer-parent-buffer
		(undo-tree-diff node)))
	(display-buffer-mark-dedicated 'soft)
	win)
    (setq win (split-window))
    (set-window-buffer win buff)
    (shrink-window-if-larger-than-buffer win)))


(defun undo-tree-visualizer-hide-diff ()
  ;; hide visualizer diff display
  (setq undo-tree-visualizer-diff nil)
  (let ((win (get-buffer-window undo-tree-diff-buffer-name)))
    (when win (with-selected-window win (kill-buffer-and-window)))))


(defun undo-tree-diff (&optional node)
  ;; Create diff between current state and NODE (or previous state, if NODE is
  ;; null). Returns buffer containing diff.
  (let (tmpfile buff)
    ;; generate diff
    (let ((undo-tree-inhibit-kill-visualizer t)
	  (current (undo-tree-current buffer-undo-tree)))
      (undo-tree-set (or node (undo-tree-node-previous current) current)
		     'preserve-timestamps)
      (setq tmpfile (diff-file-local-copy (current-buffer)))
      (undo-tree-set current 'preserve-timestamps))
    (setq buff (diff-no-select
		(current-buffer) tmpfile nil 'noasync
		(get-buffer-create undo-tree-diff-buffer-name)))
    ;; delete process messages and useless headers from diff buffer
    (with-current-buffer buff
      (goto-char (point-min))
      (delete-region (point) (1+ (line-end-position 3)))
      (goto-char (point-max))
      (forward-line -2)
      (delete-region (point) (point-max))
      (setq cursor-type nil)
      (setq buffer-read-only t))
    buff))


(defun undo-tree-visualizer-update-diff (&optional node)
  ;; update visualizer diff display to show diff between current state and
  ;; NODE (or previous state, if NODE is null)
  (with-current-buffer undo-tree-visualizer-parent-buffer
    (undo-tree-diff node))
  (let ((win (get-buffer-window undo-tree-diff-buffer-name)))
    (when win
      (balance-windows)
      (shrink-window-if-larger-than-buffer win))))



(provide 'undo-tree)

;;; undo-tree.el ends here
