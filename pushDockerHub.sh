echo "push to the dockerhub"
docker build --build-arg CACHEBUST=$(date +%s) .
docker images
echo "docker tag <tag> pooja2012/CodeComplexity"
echo "docker push pooja2012/CodeComplexity"
