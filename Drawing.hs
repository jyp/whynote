module Drawing where
import Event

type Stroke = [PointerCoord]
type Drawing = [Stroke]

emptyDrawing = []
