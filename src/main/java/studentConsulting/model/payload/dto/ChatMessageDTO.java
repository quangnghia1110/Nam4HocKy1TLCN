package studentConsulting.model.payload.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ChatMessageDTO {
    private Integer conversationId;
    private Integer fromUserId;
    private String content;
    private String type;
    private Boolean statusRead;
    private Boolean statusSend;
    private Boolean statusRecall;
    private LocalDateTime sentAt;
}
