package solutions.s4y.verba.http

import munit.FunSuite
import com.comcast.ip4s.{Ipv4Address, Port}

class ServerConfigSuite extends FunSuite {

  test("fromArgs returns valid config with all arguments provided") {
    val args = List("--host", "127.0.0.1", "--port", "9090", "--secret", "my-secret")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Right(config) =>
        assertEquals(config.host, Ipv4Address.fromString("127.0.0.1").get)
        assertEquals(config.port, Port.fromInt(9090).get)
        assertEquals(config.secret, "my-secret")
      case Left(error) =>
        fail(s"Expected valid config but got error: $error")
    }
  }

  test("fromArgs uses default host and port when not provided") {
    val args = List("--secret", "my-secret")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Right(config) =>
        assertEquals(config.host, Ipv4Address.fromBytes(0, 0, 0, 0))
        assertEquals(config.port, Port.fromInt(8080).get)
        assertEquals(config.secret, "my-secret")
      case Left(error) =>
        fail(s"Expected valid config but got error: $error")
    }
  }

  test("fromArgs accepts short flags") {
    val args = List("-h", "192.168.1.1", "-p", "3000", "-s", "secret123")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Right(config) =>
        assertEquals(config.host, Ipv4Address.fromString("192.168.1.1").get)
        assertEquals(config.port, Port.fromInt(3000).get)
        assertEquals(config.secret, "secret123")
      case Left(error) =>
        fail(s"Expected valid config but got error: $error")
    }
  }

  test("fromArgs fails when secret is missing") {
    val args = List("--host", "127.0.0.1", "--port", "8080")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Secret is required"))
      case Right(_) =>
        fail("Expected error for missing secret")
    }
  }

  test("fromArgs accumulates multiple validation errors") {
    val args = List("--host", "invalid-host", "--port", "99999", "--unknown", "value")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Invalid IPv4 address: invalid-host"))
        assert(error.contains("Invalid port: 99999"))
        assert(error.contains("Unknown argument: --unknown"))
        assert(error.contains("Secret is required"))
      case Right(_) =>
        fail("Expected multiple validation errors")
    }
  }

  test("fromArgs reports invalid IPv4 address") {
    val args = List("--host", "999.999.999.999", "--secret", "test")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Invalid IPv4 address: 999.999.999.999"))
      case Right(_) =>
        fail("Expected error for invalid IPv4 address")
    }
  }

  test("fromArgs reports invalid port number - negative") {
    val args = List("--port", "-1", "--secret", "test")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Invalid port: -1"))
      case Right(_) =>
        fail("Expected error for invalid port")
    }
  }

  test("fromArgs reports invalid port - too large") {
    val args = List("--port", "70000", "--secret", "test")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Invalid port: 70000"))
      case Right(_) =>
        fail("Expected error for port > 65535")
    }
  }

  test("fromArgs reports invalid port - not a number") {
    val args = List("--port", "abc", "--secret", "test")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Invalid port: abc"))
      case Right(_) =>
        fail("Expected error for non-numeric port")
    }
  }

  test("fromArgs reports unknown arguments") {
    val args = List("--secret", "test", "--unknown", "value", "--another", "bad")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Unknown argument: --unknown"))
        assert(error.contains("Unknown argument: --another"))
      case Right(_) =>
        fail("Expected errors for unknown arguments")
    }
  }

  test("fromArgs reports missing value for host flag") {
    val args = List("--host")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Missing value for --host"))
        assert(error.contains("Secret is required"))
      case Right(_) =>
        fail("Expected error for missing host value")
    }
  }

  test("fromArgs reports missing value for port flag") {
    val args = List("--port", "--secret", "test")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        // --secret is interpreted as value for --port (invalid)
        // then we miss the secret
        assert(error.contains("Invalid port: --secret") || error.contains("Unknown argument"))
      case Right(_) =>
        fail("Expected error")
    }
  }

  test("fromArgs handles mixed valid and invalid arguments") {
    val args = List(
      "--host", "10.0.0.1",
      "--port", "invalid-port",
      "--unknown-flag", "value",
      "--secret", "test-secret"
    )
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Invalid port: invalid-port"))
        assert(error.contains("Unknown argument: --unknown-flag"))
      case Right(_) =>
        fail("Expected errors for invalid port and unknown flag")
    }
  }

  test("fromArgs accepts valid port boundaries") {
    val args1 = List("--port", "1", "--secret", "test")
    val result1 = ServerConfig.fromArgs(args1)
    assert(result1.isRight, "Port 1 should be valid")

    val args2 = List("--port", "65535", "--secret", "test")
    val result2 = ServerConfig.fromArgs(args2)
    assert(result2.isRight, "Port 65535 should be valid")
  }

  test("fromArgs handles empty arguments list") {
    val args = List.empty[String]
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Secret is required"))
      case Right(_) =>
        fail("Expected error for missing secret")
    }
  }

  test("fromArgs last value wins for duplicate flags") {
    val args = List(
      "--host", "127.0.0.1",
      "--host", "192.168.1.1",
      "--port", "8080",
      "--port", "9090",
      "--secret", "first",
      "--secret", "second"
    )
    val result = ServerConfig.fromArgs(args)

    result match {
      case Right(config) =>
        assertEquals(config.host, Ipv4Address.fromString("192.168.1.1").get)
        assertEquals(config.port, Port.fromInt(9090).get)
        assertEquals(config.secret, "second")
      case Left(error) =>
        fail(s"Expected valid config but got error: $error")
    }
  }

  test("fromArgs handles IPv4 0.0.0.0") {
    val args = List("--host", "0.0.0.0", "--secret", "test")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Right(config) =>
        assertEquals(config.host, Ipv4Address.fromBytes(0, 0, 0, 0))
      case Left(error) =>
        fail(s"Expected valid config but got error: $error")
    }
  }

  test("fromArgs handles complex secret strings") {
    val complexSecret = "my-secret-123!@#$%^&*()_+-=[]{}|;:',.<>?"
    val args = List("--secret", complexSecret)
    val result = ServerConfig.fromArgs(args)

    result match {
      case Right(config) =>
        assertEquals(config.secret, complexSecret)
      case Left(error) =>
        fail(s"Expected valid config but got error: $error")
    }
  }

  test("fromArgs handles missed port value") {
    val complexSecret = "my-secret-123!@#$%^&*()_+-=[]{}|;:',.<>?"
    val args = List("--secret", complexSecret, "--port")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Missing value for --port"))
      case Right(config) =>
        fail(s"Expected error for missing port value but got config: $config")
    }
  }

  test("fromArgs handles missed both port value and secret") {
    val complexSecret = "my-secret-123!@#$%^&*()_+-=[]{}|;:',.<>?"
    val args = List("--secret", complexSecret, "--port")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Missing value for --port s"))
      case Right(config) =>
        fail(s"Expected error for missing port value but got config: $config")
    }
  }

  test("fromArgs handles both invalid port value and missed secret") {
    val args = List("--port", "abc")
    val result = ServerConfig.fromArgs(args)

    result match {
      case Left(error) =>
        assert(error.contains("Invalid port: abc (must be 1-65535)\nSecret is required"))
      case Right(config) =>
        fail(s"Expected error for missing port value but got config: $config")
    }
  }
}

