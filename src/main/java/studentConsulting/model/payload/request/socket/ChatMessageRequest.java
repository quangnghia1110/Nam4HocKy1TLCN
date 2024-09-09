package studentConsulting.model.payload.request.socket;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ChatMessageRequest {
    private Integer conversationId;
    private Integer fromUserId;
    private Integer advisorId;   
    private String content;
    private String type;
}
