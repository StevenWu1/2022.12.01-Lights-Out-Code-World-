import CodeWorld
import qualified Data.Text as T
type Model = [Point]

nextColor x = 3 - x

circleH x y = colored black $ translated (x) (y) $ solidCircle 0.5

blankBoard = [translated a b $ circle 0.5 | a<-[(-2)..2], b<-[(-2)..2]]
--render board = pictures (blankBoard++[coordinatePlane])
--render board = pictures ((map (\(a, b) -> circleH a b 1) board) ++ [coordinatePlane])

distances = [-2..2]
smallestDistance x = head [ xs | xs <- distances, abs(xs - x) < 0.5]

clickedPt :: Point -> Maybe Point
clickedPt (x, y)
 | x < -2.5 || x > 2.5 || y < -2.5 || y > 2.5 = Nothing
 | otherwise = Just (smallestDistance x, smallestDistance y)

flipOne :: Model -> Point -> Model
flipOne m p
 | p `elem` m = filter (/= p) m
 | otherwise = p:m

flipAll ::Model -> [Point] -> Model
flipAll m [] = m
flipAll m (p:xs) = flipAll (flipOne m p) xs

renderH [] = []
renderH ((x, y):xs) = circleH x y : renderH xs
render :: Model -> Picture
render m = pictures ((renderH m) ++ blankBoard ++ [coordinatePlane])

pointsAroundx (x, y) = [ (x+a, y) | a<-[-1, 1], x+a <= 2, -2 <= x+a]
pointsAroundy (x, y) = [ (x, y+b) | b<-[-1, 1], y+b <= 2, -2 <= y+b]
pointsAround (x, y) = (x, y) : pointsAroundx (x, y) ++ pointsAroundy (x, y)
updateClicks :: Model -> Maybe Point -> Model
updateClicks m Nothing = m
updateClicks m (Just x) = flipAll m (pointsAround x)

updateModel :: Event -> Model -> Model
updateModel (PointerPress(p)) x = updateClicks x (clickedPt p)
updateModel _ x = x

main = activityOf [] updateModel render
