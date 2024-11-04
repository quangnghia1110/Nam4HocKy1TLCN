package studentConsulting.model.payload.dto.common;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@Builder
public class NotificationDTO {
    private Integer id;
    private SenderDTO sender;
    private String content;
    private LocalDateTime time;

    @Data
    @Builder
    public static class SenderDTO {
        private Integer id;
        private String email;
        private String fullName;
    }
}

