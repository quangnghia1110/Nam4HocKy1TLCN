package studentConsulting.model.payload.dto;

import java.time.LocalDateTime;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConversationDTO {
	private Integer id;
	private Integer departmentId;
    private String userName;  
    private String consultantName;
    private String name;
    private Boolean isGroup;
    private LocalDateTime createdAt;
}
