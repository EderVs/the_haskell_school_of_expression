module Shape where

data Shape = Rectangle Side Side
		   | Ellipse Radius Radius
		   | RtTriangle Side Side
		   | Polygon [Vertex]
	deriving Show

type Side = Float
type Radius = Float
type Vertex = (Float, Float)
type Angle = Float

square :: Side -> Shape
square s = Rectangle s s

circle :: Radius -> Shape
circle r = Ellipse r r

rectangle :: Shape -> Shape
rectangle (Polygon [v1, v2, v3, v4]) = Rectangle (distance v1 v2) (distance v2 v3)

-- Where v3-v1 is the hypotenuse
rtTriangle :: Shape -> Shape
rtTriangle (Polygon [v1, v2, v3]) = RtTriangle (distance v1 v2) (distance v2 v3)

getJustVertex :: Angle -> Radius -> Vertex
getJustVertex a r = (r * (cos a), r * (sin a))

getRegularPolygonVertex :: Angle -> Angle -> Radius -> Int -> [Vertex]
getRegularPolygonVertex _ _ _ 0 = []
getRegularPolygonVertex i a r n = (getJustVertex i r):getRegularPolygonVertex (i+a) a r (n-1)

-- Regular Polygon of n sides of l.
regularPolygon :: Int -> Side -> Shape
regularPolygon n l = Polygon ((r,0):getRegularPolygonVertex angle angle r (n-1))
	where
		angle = (2 * pi / (fromIntegral n))
		r = l/2/(sin (angle/2))

distance :: Vertex -> Vertex -> Float
distance (v1x, v1y) (v2x, v2y) = sqrt((v1x-v2x)^2 + (v1y-v2y)^2)