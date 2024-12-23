
```sh

./mill clean

./mill __.reformat

./mill __.test

./mill __.publishLocal
```

```sh

# RELEASE
# bump publishVersion to x.y.z !!!
$VERSION="x.y.z"
git commit --allow-empty -am "Release $VERSION"
git tag -a $VERSION -m "Release $VERSION"
git push --atomic origin main --tags


# prepare for NEXT version
# bump publishVersion to x.y.z-SNAPSHOT
$VERSION="x.y.z-SNAPSHOT"
git commit -am"Bump version to $VERSION"

```
