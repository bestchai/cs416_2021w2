package main

import (
	"encoding/json"
	"fmt"
	"github.com/DistributedClocks/tracing"
	"io/ioutil"
	"os"
	"strconv"
)

/** Config struct **/

type ClientConfig struct {
	ClientAddress        string
	NimServerAddressList []string // Maximum 8 nim servers will be provided
	TracingServerAddress string
	Secret               []byte
	TracingIdentity      string
	// FCheck stuff:
	FCheckAckLocalAddr   string
	FCheckHbeatLocalAddr string
	FCheckLostMsgsThresh uint8
}

/** Tracing structs **/

type GameStart struct {
	Seed int8
}

type ClientMove StateMoveMessage

type ServerMoveReceive StateMoveMessage

type GameComplete struct {
	Winner string
}

/** New tracing structs introduced in A2 **/

type NewNimServer struct {
	NimServerAddress string
}

type NimServerFailed struct {
	NimServerAddress string
}

type AllNimServersDown struct {
}

/** Message structs **/

type StateMoveMessage struct {
	GameState         []uint8
	MoveRow           int8
	MoveCount         int8
	TracingServerAddr string               // ADDED IN A2
	Token             tracing.TracingToken // ADDED IN A2
}

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage: client [seed]")
		return
	}
	arg, err := strconv.Atoi(os.Args[1])
	CheckErr(err, "Provided seed could not be converted to integer", arg)
	seed := int8(arg)

	config := ReadConfig("./config/client_config.json")

	// now connect to it
	tracer := tracing.NewTracer(tracing.TracerConfig{
		ServerAddress:  config.TracingServerAddress,
		TracerIdentity: config.TracingIdentity,
		Secret:         config.Secret,
	})
	defer tracer.Close()

	trace := tracer.CreateTrace()
	trace.RecordAction(
		GameStart{
			Seed: seed,
		})

	// Your code goes from here
}

func ReadConfig(filepath string) *ClientConfig {
	configFile := filepath
	configData, err := ioutil.ReadFile(configFile)
	CheckErr(err, "reading config file")

	config := new(ClientConfig)
	err = json.Unmarshal(configData, config)
	CheckErr(err, "parsing config data")

	return config
}

func CheckErr(err error, errfmsg string, fargs ...interface{}) {
	if err != nil {
		fmt.Fprintf(os.Stderr, errfmsg, fargs...)
		os.Exit(1)
	}
}
