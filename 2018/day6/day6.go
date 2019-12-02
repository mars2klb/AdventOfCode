package main

import (
	"bytes"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"math"
	"strconv"
)

type Point struct {
	x int
	y int
}

type Marker struct {
	id       string
	distance int
}

type Grid struct {
	size *Point
	grid [][]Marker
}

func makeGrid(sz *Point) *Grid {
	maxx := sz.x + 1
	maxy := sz.y + 1

	g := make([][]Marker, maxx)
	for y, _ := range g {
		g[y] = make([]Marker, maxy)
	}

	return &Grid{
		size: &Point{maxx, maxy},
		grid: g,
	}
}

func (p *Point) distance(op Point) int {
	// fmt.Printf("dx: %v %v\n", p1, p2)
	return int(math.Abs(float64(p.x)-float64(op.x)) + math.Abs(float64(p.y)-float64(op.y)))
}

func (p *Point) equal(op Point) bool {
	return p.x == op.x && p.y == op.y
}

func (p *Point) tag() string {
	return fmt.Sprintf("(%d,%d)", p.x, p.y)
}

// ************************************************* DAY 1 *************************************************

func (g *Grid) mark(origin Point) {
	grid := g.grid
	for x, row := range g.grid {
		for y, _ := range row {
			this := Point{x: x, y: y}
			distance := origin.distance(this)

			if this.equal(origin) {
				grid[x][y] = Marker{origin.tag(), -1}
			} else if grid[x][y].distance == 0 || distance < grid[x][y].distance {
				// fmt.Printf(" REPLACE origin: %v  point: %v  new dx: %v  old dx: %v\n", id, Point{x, y}, distance, grid[x][y].distance)
				grid[x][y] = Marker{origin.tag(), distance}
			} else if grid[x][y].distance == distance {
				// fmt.Printf(" NULLIFY origin: %v  point: %v  new dx: %v  old dx: %v\n", id, Point{x, y}, distance, grid[x][y].distance)
				grid[x][y] = Marker{".", distance}
			}
		}
	}
}

func (g *Grid) tally() {
	notes := make(map[string]int)

	for x, row := range g.grid {
		for y, col := range row {
			if x == 0 || x == g.size.x-1 || y == 0 || y == g.size.y-1 {
				notes[col.id] = notes[col.id] - 1000
			}
			notes[col.id] = notes[col.id] + 1
		}
	}

	max := 0
	id := ""
	for m, p := range notes {
		if p > max && m != "*" {
			max = p
			id = m
		}
	}

	fmt.Printf("MAX: %s: %d\n", id, max)
}

func day1(grid *Grid, origins []Point) {
	for _, origin := range origins {
		grid.mark(origin)
	}

	grid.tally()
	// grid.dump()
}

// ************************************************* DAY 2 *************************************************
type MarkedPoint struct {
	Point
	Marker
}

func tally2(grid *Grid) int {
	count := 0
	for x := 0; x < grid.size.x; x++ {
		for y := 0; y < grid.size.y; y++ {
			if grid.grid[x][y].id == "*" {
				count++
			}
		}
	}

	return count
}

func day2(grid *Grid, origins []Point, limit int) {
	markchan := make(chan MarkedPoint, 1000)
	compute := func(p Point) {
		distance := 0
		for _, o := range origins {
			distance += p.distance(o)
		}

		markchan <- MarkedPoint{
			Point: Point{
				x: p.x,
				y: p.y,
			},
			Marker: Marker{
				id:       p.tag(),
				distance: distance,
			},
		}
	}

	for x := 0; x < grid.size.x; x++ {
		for y := 0; y < grid.size.y; y++ {
			go compute(Point{x, y})
		}
	}

	for left := grid.size.x * grid.size.y; 0 < left; left-- {
		marked := <-markchan
		if marked.Marker.distance < limit {
			grid.grid[marked.x][marked.y].id = "*"
		}
	}

	// grid.dump()
	fmt.Printf("Region size: %d\n", tally2(grid))
}

// ************************************************* COMMON *************************************************

func (g *Grid) dump() {
	tags := make(map[string]string)
	for y := 0; y < g.size.y; y++ {
		for x := 0; x < g.size.x; x++ {
			m := g.grid[x][y]
			if _, ok := tags[m.id]; !ok {
				if m.id == "." {
					tags[m.id] = "."
				} else if m.id == "*" {
					tags[m.id] = "*"
				} else {
					tags[m.id] = fmt.Sprintf("%d", len(tags))
				}
			}
			fmt.Print(tags[m.id])
			// fmt.Printf("(%d|%d|%d)", x, g.size.x, x%g.size.x)
			if x == g.size.x-1 {
				fmt.Printf("\n")
			}
			// if x > 0 && x%g.size.x == 0 {
			// 	fmt.Printf("\n")
			// }
		}
	}

	fmt.Println(tags)
}

func size(origins []Point) *Point {
	maxx := 0
	maxy := 0

	for _, point := range origins {
		if point.x > maxx {
			maxx = point.x
		}

		if point.y > maxy {
			maxy = point.y
		}
	}

	return &Point{x: maxx, y: maxy}
}

func main() {
	flag.Parse()
	b, err := ioutil.ReadFile(flag.Arg(0))
	if err != nil {
		log.Fatalln(err)
	}

	var x, y int
	entries := bytes.Split(b, []byte("\n"))
	origins := make([]Point, 0)

	for _, line := range entries {
		if len(line) == 0 {
			continue
		}

		_, err := fmt.Sscanf(string(line), "%d, %d\n", &x, &y)
		if err != nil {
			log.Fatalln(err)
		}
		// fmt.Printf("x: %v  y: %v\n", x, y)
		origins = append(origins, Point{x: x, y: y})
	}

	s := size(origins)
	grid := makeGrid(s)
	fmt.Println("size:", s)

	if len(flag.Args()) == 1 {
		day1(grid, origins)
	} else {
		limit, err := strconv.Atoi(flag.Arg(1))
		if err != nil {
			log.Fatalln(err)
		}
		day2(grid, origins, limit)
	}
}
