package com.cardsvc;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.context.annotation.Bean;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.test.context.ContextConfiguration;
import org.testcontainers.containers.PostgreSQLContainer;

@SpringBootTest(properties = "spring.batch.job.enabled=false")
@ContextConfiguration(classes = {CardsvcApplication.class, CardsvcApplicationTests.TestContainersConfig.class})
class CardsvcApplicationTests {

    @TestConfiguration(proxyBeanMethods = false)
    static class TestContainersConfig {
        @Bean
        @ServiceConnection
        PostgreSQLContainer<?> postgres() {
            return new PostgreSQLContainer<>("postgres:16");
        }
    }

    @Test
    void contextLoads() {
        // Phase 0 exit criterion: the app builds and the (empty) suite runs green
        // against a real PostgreSQL 16 with Flyway applied.
    }
}
