package com.cardsvc.common;

import java.util.ArrayList;
import java.util.List;

import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ExitCodeGenerator;
import org.springframework.boot.autoconfigure.batch.JobExecutionEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

/**
 * Maps job outcomes to the estate return-code convention
 * (0 OK, 4 warning/partial, 8 reserved business outcomes, 12 fatal).
 *
 * Numeric exit codes are carried in the job's {@link ExitStatus} exit code
 * (e.g. "RC4"); each job's FR doc defines its own mapping, and reserved
 * business RCs (e.g. CBCRD08 RC 8) are never produced by generic failures,
 * which map to 12.
 */
@Component
public class EstateExitCodeMapper implements ExitCodeGenerator, ApplicationListener<JobExecutionEvent> {

    private static final Logger log = LoggerFactory.getLogger(EstateExitCodeMapper.class);

    private final List<JobExecution> executions = new ArrayList<>();

    @Override
    public void onApplicationEvent(JobExecutionEvent event) {
        executions.add(event.getJobExecution());
    }

    @Override
    public int getExitCode() {
        if (executions.isEmpty()) {
            // A launch that ran no job is a failure, like a JCL step whose
            // program never executed.
            log.error("RC=12: no batch job execution was recorded for this launch "
                    + "(missing or unmatched --spring.batch.job.name, or the runner never ran)");
            return 12;
        }
        int rc = 0;
        for (JobExecution execution : executions) {
            if (execution.getStatus() != BatchStatus.COMPLETED) {
                return 12;
            }
            Integer mapped = parseRc(execution.getExitStatus());
            if (mapped != null) {
                rc = Math.max(rc, mapped);
            }
        }
        return rc;
    }

    private static Integer parseRc(ExitStatus exitStatus) {
        String code = exitStatus.getExitCode();
        if (code != null && code.startsWith("RC")) {
            try {
                return Integer.parseInt(code.substring(2));
            } catch (NumberFormatException ignored) {
                return null;
            }
        }
        return null;
    }
}
