# :black_nib: Roave\Signature

Sign and validate signed files made easy.

**Note: this is not a cryptographic signing library.**

### Installation

The suggested installation method is via [composer](https://getcomposer.org/):

```bash
$ composer require roave/signature
```

### Usage examples

#### Signing a file

```php
// Creating a signer
$signer = new \Roave\Signature\FileContentSigner(
    new \Roave\Signature\Encoder\Sha1SumEncoder()
);

// It'll give you a signature to the provided code content
$signature = $signer->sign(file_get_contents('/var/tmp/file.php'));
```

#### Validating a signed file

```php
// Creating a signer checker
$signer = new \Roave\Signature\FileContentChecker(
    new \Roave\Signature\Encoder\Sha1SumEncoder()
);

// It'll validate the signature on file content
$signer->check(file_get_contents('/var/tmp/signed-file.php'));
```
