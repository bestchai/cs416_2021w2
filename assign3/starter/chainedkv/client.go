package chainedkv

type ClientConfig struct {
	ClientID              string
	CoordIPPort           string
	LocalCoordIPPort      string
	LocalHeadServerIPPort string
	LocalTailServerIPPort string
	ChCapacity            int
	TracingServerAddr     string
	Secret                []byte
	TracingIdentity       string
}
