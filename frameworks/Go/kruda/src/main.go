package main

import (
	"context"
	"math/rand/v2"
	"os"
	"sort"
	"strconv"
	"sync"
	"time"

	"github.com/go-kruda/kruda"
	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgxpool"
)

type World struct {
	ID           int32 `json:"id"`
	RandomNumber int32 `json:"randomNumber"`
}

type Fortune struct {
	ID      int32
	Message string
}

type JSONMessage struct {
	Message string `json:"message"`
}

var pool *pgxpool.Pool

// cachedWorlds holds pre-fetched worlds for cached-queries test
var cachedWorlds sync.Map

func main() {
	dsn := os.Getenv("DATABASE_URL")
	if dsn == "" {
		dsn = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?sslmode=disable"
	}

	cfg, _ := pgxpool.ParseConfig(dsn)
	cfg.MaxConns = 64
	cfg.MinConns = 8
	cfg.AfterConnect = func(ctx context.Context, conn *pgx.Conn) error {
		conn.Prepare(ctx, "worldSelect", "SELECT randomnumber FROM world WHERE id=$1")
		conn.Prepare(ctx, "fortuneSelect", "SELECT id, message FROM fortune")
		conn.Prepare(ctx, "worldUpdate", "UPDATE world SET randomnumber=$1 WHERE id=$2")
		return nil
	}

	var err error
	pool, err = pgxpool.NewWithConfig(context.Background(), cfg)
	if err != nil {
		panic(err)
	}

	// Pre-populate cache
	go populateCache()

	app := kruda.New(kruda.Wing())

	// TFB Test 1: JSON serialization
	app.Get("/json", func(c *kruda.Ctx) error {
		return c.JSON(JSONMessage{Message: "Hello, World!"})
	}, kruda.WingJSON())

	// TFB Test 2: Plaintext
	app.Get("/plaintext", func(c *kruda.Ctx) error {
		return c.Text("Hello, World!")
	}, kruda.WingPlaintext())

	// TFB Test 3: Single database query
	app.Get("/db", func(c *kruda.Ctx) error {
		w := World{ID: int32(rand.IntN(10000) + 1)}
		pool.QueryRow(context.Background(), "worldSelect", w.ID).Scan(&w.RandomNumber)
		return c.JSON(w)
	}, kruda.WingQuery())

	// TFB Test 4: Multiple database queries
	app.Get("/queries", func(c *kruda.Ctx) error {
		n := clamp(queryParam(c, "n"), 1, 500)
		worlds := make([]World, n)
		for i := range worlds {
			worlds[i].ID = int32(rand.IntN(10000) + 1)
		}
		batch := &pgx.Batch{}
		for i := range worlds {
			batch.Queue("worldSelect", worlds[i].ID)
		}
		br := pool.SendBatch(context.Background(), batch)
		for i := range worlds {
			br.QueryRow().Scan(&worlds[i].RandomNumber)
		}
		br.Close()
		return c.JSON(worlds)
	}, kruda.WingQuery())

	// TFB Test 5: Fortunes
	app.Get("/fortunes", func(c *kruda.Ctx) error {
		rows, err := pool.Query(context.Background(), "fortuneSelect")
		if err != nil {
			return c.Status(500).Text(err.Error())
		}
		defer rows.Close()

		fortunes := make([]Fortune, 0, 13)
		for rows.Next() {
			var f Fortune
			rows.Scan(&f.ID, &f.Message)
			fortunes = append(fortunes, f)
		}
		fortunes = append(fortunes, Fortune{Message: "Additional fortune added at request time."})
		sort.Slice(fortunes, func(i, j int) bool { return fortunes[i].Message < fortunes[j].Message })

		return c.HTML(fortunesHTML(fortunes))
	}, kruda.WingRender())

	// TFB Test 6: Database updates
	app.Get("/updates", func(c *kruda.Ctx) error {
		n := clamp(queryParam(c, "n"), 1, 500)
		worlds := make([]World, n)
		for i := range worlds {
			worlds[i].ID = int32(rand.IntN(10000) + 1)
		}
		batch := &pgx.Batch{}
		for i := range worlds {
			batch.Queue("worldSelect", worlds[i].ID)
		}
		br := pool.SendBatch(context.Background(), batch)
		for i := range worlds {
			br.QueryRow().Scan(&worlds[i].RandomNumber)
			worlds[i].RandomNumber = int32(rand.IntN(10000) + 1)
		}
		br.Close()

		ids := make([]int32, n)
		nums := make([]int32, n)
		for i, w := range worlds {
			ids[i] = w.ID
			nums[i] = w.RandomNumber
		}
		pool.Exec(context.Background(),
			"UPDATE world SET randomnumber=v.r FROM (SELECT unnest($1::int[]) id, unnest($2::int[]) r) v WHERE world.id=v.id",
			ids, nums,
		)
		return c.JSON(worlds)
	}, kruda.WingQuery())

	// TFB Test 7: Cached queries
	app.Get("/cached-queries", func(c *kruda.Ctx) error {
		n := clamp(queryParam(c, "n"), 1, 500)
		worlds := make([]World, n)
		for i := range worlds {
			id := int32(rand.IntN(10000) + 1)
			if w, ok := cachedWorlds.Load(id); ok {
				worlds[i] = w.(World)
			} else {
				worlds[i] = World{ID: id}
				pool.QueryRow(context.Background(), "worldSelect", id).Scan(&worlds[i].RandomNumber)
				cachedWorlds.Store(id, worlds[i])
			}
		}
		return c.JSON(worlds)
	}, kruda.WingQuery())

	app.Listen(":8080")
}

func populateCache() {
	// Wait for DB to be ready, then refresh cache periodically
	for {
		time.Sleep(5 * time.Second)
		batch := &pgx.Batch{}
		for i := int32(1); i <= 10000; i++ {
			batch.Queue("worldSelect", i)
		}
		br := pool.SendBatch(context.Background(), batch)
		for i := int32(1); i <= 10000; i++ {
			var rn int32
			if err := br.QueryRow().Scan(&rn); err == nil {
				cachedWorlds.Store(i, World{ID: i, RandomNumber: rn})
			}
		}
		br.Close()
	}
}

func queryParam(c *kruda.Ctx, name string) int {
	n, _ := strconv.Atoi(c.Query(name))
	return n
}

func clamp(n, lo, hi int) int {
	if n < lo {
		return lo
	}
	if n > hi {
		return hi
	}
	return n
}

var fortuneBufPool = sync.Pool{
	New: func() any { b := make([]byte, 0, 4096); return &b },
}

func fortunesHTML(ff []Fortune) string {
	bp := fortuneBufPool.Get().(*[]byte)
	buf := (*bp)[:0]
	buf = append(buf, "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"...)
	for _, f := range ff {
		buf = append(buf, "<tr><td>"...)
		buf = strconv.AppendInt(buf, int64(f.ID), 10)
		buf = append(buf, "</td><td>"...)
		buf = appendHTMLEscape(buf, f.Message)
		buf = append(buf, "</td></tr>"...)
	}
	buf = append(buf, "</table></body></html>"...)
	s := string(buf)
	*bp = buf
	fortuneBufPool.Put(bp)
	return s
}

func appendHTMLEscape(buf []byte, s string) []byte {
	last := 0
	for i := 0; i < len(s); i++ {
		var esc string
		switch s[i] {
		case '&':
			esc = "&amp;"
		case '<':
			esc = "&lt;"
		case '>':
			esc = "&gt;"
		case '"':
			esc = "&#34;"
		case '\'':
			esc = "&#39;"
		default:
			continue
		}
		buf = append(buf, s[last:i]...)
		buf = append(buf, esc...)
		last = i + 1
	}
	return append(buf, s[last:]...)
}
