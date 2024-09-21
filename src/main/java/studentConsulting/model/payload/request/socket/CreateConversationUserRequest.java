package studentConsulting.model.payload.request.socket;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateConversationUserRequest {
    private Integer departmentId;    
    private Integer consultantId; 
}
