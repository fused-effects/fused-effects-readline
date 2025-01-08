# v0.2.0.0

- Removes `prettyprinter` dependency.

- Removes the styling applied to prompts; users can add styling to the prompt for themselves if so desired.

- Removes line number tracking.

- Removes `print` in favour of `outputStr` & `outputStrLn`.

- Renames `prompt` to `getInputLine`, matching `haskeline`’s naming.

- Defines `getInputLineWithInitial`, `getInputChar`, `getPassword`, etc. operations matching `haskeline`’s API.

- Allows other effects to be lifted through `ReadlineC` on `haskeline` 0.8.1 or later. ([#3](https://github.com/fused-effects/fused-effects-readline/issues/3))


# v0.1.0.1

- Support for `prettyprinter` 1.7.


# v0.1.0.0

- Support for `ghc` 8.10.
- Support for `haskeline` 0.8.
- Support for `fused-effects` 1.1.


# v0.0.0.0

Initial release.
