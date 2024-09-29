package studentConsulting.model.payload.dto.notification;

import java.time.LocalDateTime;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class NotificationResponseDTO {
    private String status; 
    private NotificationDTO data; 

    @Data
    @Builder
    public static class NotificationDTO {
        private Integer senderId;
        private Integer receiverId;
        private String content;
        private LocalDateTime time;
        private String notificationType; 
        private String status; 
    }
}

