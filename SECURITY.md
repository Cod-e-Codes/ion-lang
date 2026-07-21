# Security policy

Ion is `0.x`. Please treat the compiler, generated C, and runtime as suitable for
experimentation and early adopters, not for security-critical deployments.

## Supported versions

Only the current `main` branch and the latest tagged `0.x` release receive
security attention.

## Reporting a vulnerability

Open a private security advisory if the hosting platform supports it. Otherwise,
contact the project maintainer privately before publishing details.

Useful reports include:

- the Ion source that triggers the issue;
- generated C, if relevant;
- compiler version or commit SHA;
- platform, C compiler, and sanitizer output;
- whether the issue is in safe Ion, `unsafe` Ion, FFI, generated C, or runtime C.

## Scope

In scope:

- memory unsafety or data races reachable from safe Ion;
- compiler crashes on valid or hostile input;
- generated C that invokes undefined behavior for safe Ion programs;
- runtime ownership, drop, channel, or spawn bugs.

Out of scope:

- undefined behavior caused by user code inside `unsafe`;
- vulnerabilities in external C libraries called through FFI;
- denial-of-service from intentionally enormous source files unless it exposes a
  distinct compiler bug.
