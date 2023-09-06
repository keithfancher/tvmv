# Release checklist

All manual steps for now. Will automate when it gets annoying!

- [ ] Run `stack test` again, just for good measure.
- [ ] Update version string in `package.yaml` (and run `stack build`
  afterwards to update the auto-generated `cabal` file as well).
- [ ] Update version string in `app/Version.hs`.
- [ ] Add new version string and date to `CHANGELOG.md` (rename the
  "Unreleased" section).
- [ ] Commit the above changes, then create and push a `git tag` at that
  commit for the new version.
- [ ] Build binary releases: Linux, Mac, and Windows. Include the following
  in the release archives (tarballs for Mac and Linux, zip file for Windows):
    1. The `tvmv` binary, of course.
    2. Some docs: `README.md`, `CHANGELOG.md`, and `LICENSE`.
- [ ] Laugh and clap your hands.
