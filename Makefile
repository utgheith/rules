JVM=graalvm-java21:21
T = compile fmt publish run test setup-ide

all : compile

${T}: % :
	scala-cli --power $@ --jvm ${JVM} .

native:
	scala-cli --power package --jvm ${JVM} . -o arm -f --native-image --graalvm-jvm-id ${JVM} .

publish_local:
	scala-cli --power publish local --jvm ${JVM} .


clean:
	scala-cli clean .


