JVM=graalvm-java21:21
T = compile fmt run test setup-ide

all : compile

${T}: % :
	scala-cli --power $@ --jvm ${JVM} src

native:
	scala-cli --power package --jvm ${JVM} src -o arm -f --native-image --graalvm-jvm-id ${JVM}

publish_local:
	scala-cli --power publish local --jvm ${JVM} src

clean:
	scala-cli clean src


