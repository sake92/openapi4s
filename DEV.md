
TODOs:
- support default values
- add more tests https://github.com/swagger-api/swagger-parser/blob/master/modules/swagger-parser-v3/src/test/resources


```sh

./mill clean

./mill __.reformat

./mill __.test

./mill __.publishLocal
```

```sh

# RELEASE
# bump publishVersion to x.y.z !!!
$VERSION="0.1.0"
git commit --allow-empty -m "Release $VERSION"
git tag -a $VERSION -m "Release $VERSION"
git push --atomic origin main --tags


# prepare for NEXT version
# bump publishVersion to x.y.z-SNAPSHOT
$VERSION="0.1.1-SNAPSHOT"
git commit -m"Bump version to $VERSION"

```
