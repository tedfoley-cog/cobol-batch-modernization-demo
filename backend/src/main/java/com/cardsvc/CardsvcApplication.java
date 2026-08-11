package com.cardsvc;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * CARDSVC batch backend. Jobs are launched one at a time via the CLI seam:
 *
 * <pre>
 *   java -jar cardsvc-backend.jar \
 *     --spring.batch.job.name=&lt;job&gt; cycleDate=&lt;CCYYMMDD&gt; [job-specific params]
 * </pre>
 *
 * The process exit code carries the estate return-code convention (0/4/8/12)
 * through Spring Boot's {@code ExitCodeGenerator} chain.
 */
@SpringBootApplication
public class CardsvcApplication {

    private static final Logger log = LoggerFactory.getLogger(CardsvcApplication.class);

    public static void main(String[] args) {
        try {
            System.exit(SpringApplication.exit(SpringApplication.run(CardsvcApplication.class, args)));
        } catch (Throwable failure) {
            log.error("RC=12: launch failed before a job execution could be recorded", failure);
            System.exit(12);
        }
    }
}
