const CIRCLE_CLASS = 'circle'
const SQUARE_CLASS = 'square'
const X_POS = 4
const Y_POS = 76
const BOARD_SIZE = 9

const cellElements = document.querySelectorAll('[data-cell]')
const winningMessageElement = document.getElementById('winningMessage')
const restartButton = document.getElementById('restartButton')
const winningMessageTextElement = document.querySelector('[data-winning-message-text]')
let circleTurn
let current_X
let current_Y


startGame()

restartButton.addEventListener('click', startGame)
cellElements.forEach(cell => {
    cell.addEventListener('click', handleClick)
})

function startGame() {
    circleTurn = true
    cellElements.forEach(cell => {
        cell.classList.remove(SQUARE_CLASS)
        cell.classList.remove(CIRCLE_CLASS)
    })
    current_X = X_POS
    current_Y = Y_POS
    cellElements[current_X].classList.add(CIRCLE_CLASS)
    cellElements[current_Y].classList.add(SQUARE_CLASS)
    winningMessageElement.classList.remove('show')
}

function handleClick(e) {
    const cell = e.target
    const currentClass = circleTurn ? CIRCLE_CLASS : SQUARE_CLASS
    var moveIndex = index(cellElements, cell)
    console.log(current_X, current_Y)
    if (document.getElementById('move').checked) {
        if (currentClass == CIRCLE_CLASS) {
            if (moveIndex - current_X == BOARD_SIZE) {
                placeMarker(cell.style.borderTopColor, cell, currentClass, moveIndex)
            } else if (current_X - moveIndex == BOARD_SIZE) {
                placeMarker(cell.style.borderBottomColor, cell, currentClass, moveIndex)
            } else if (current_X - moveIndex == 1) {
                placeMarker(cell.style.borderRightColor, cell, currentClass, moveIndex)
            } else if (moveIndex - current_X == 1) {
                placeMarker(cell.style.borderLeftColor, cell, currentClass, moveIndex)
            }
        } else {
            if (moveIndex - current_Y == BOARD_SIZE) {
                placeMarker(cell.style.borderTopColor, cell, currentClass, moveIndex)
            } else if (current_Y - moveIndex == BOARD_SIZE) {
                placeMarker(cell.style.borderBottomColor, cell, currentClass, moveIndex)
            } else if (current_Y - moveIndex == 1) {
                placeMarker(cell.style.borderRightColor, cell, currentClass, moveIndex)
            } else if (moveIndex - current_Y == 1) {
                placeMarker(cell.style.borderLeftColor, cell, currentClass, moveIndex)
            }
        }
        // Check winning position
        if (currentClass == CIRCLE_CLASS && moveIndex > 71) {
            winningMessageTextElement.innerHTML = "Circle wins!"
            winningMessageElement.classList.add('show')
        } else if (currentClass == SQUARE_CLASS && moveIndex < 9) {
            winningMessageTextElement.innerHTML = "Square wins!"
            winningMessageElement.classList.add('show')
        }
    } else if (document.getElementById('wall_V').checked) {
        if (moveIndex % BOARD_SIZE != 8 && moveIndex > 8) {
            if (cellElements[moveIndex].style.borderRightColor != 'red' && cellElements[moveIndex - 9].style.borderRightColor != 'red') {
                cellElements[moveIndex].style.borderRightColor = 'red'
                cellElements[moveIndex - 9].style.borderRightColor = 'red'
                cellElements[moveIndex + 1].style.borderLeftColor = 'red'
                cellElements[moveIndex - 8].style.borderLeftColor = 'red'
                circleTurn = !circleTurn
            }
        }
    } else if (document.getElementById('wall_H').checked) {
        if (moveIndex % BOARD_SIZE != 8 && moveIndex > 8) {
            if (cellElements[moveIndex].style.borderTopColor != 'red' && cellElements[moveIndex + 1].style.borderRightColor != 'red') {
                cellElements[moveIndex].style.borderTopColor = 'red'
                cellElements[moveIndex + 1].style.borderTopColor = 'red'
                cellElements[moveIndex - 9].style.borderBottomColor = 'red'
                cellElements[moveIndex - 8].style.borderBottomColor = 'red'
                circleTurn = !circleTurn
            }
        }
    }
}

function index(list, target) {
    for (var i = 0; i < list.length; i++) {
        if (list[i] == target)
            return i
    }
    return -1
}


function placeMarker(borderColor, cell, currentClass, moveIndex) {
    if (borderColor != 'red') {
        cell.classList.add(currentClass)
    } else {
        return
    }
    if (currentClass == CIRCLE_CLASS) {
        cellElements[current_X].classList.remove(currentClass)
        current_X = moveIndex
    } else {
        cellElements[current_Y].classList.remove(currentClass)
        current_Y = moveIndex
    }
    circleTurn = !circleTurn
}