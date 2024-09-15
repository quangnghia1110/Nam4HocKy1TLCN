package studentConsulting.model.payload.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PostDTO {
	
    private String content;
    private Integer userId;
    private boolean isAnonymous;
    private LocalDateTime createdAt;
    private String fileName; 
    private boolean isApproved;
    private int views;
}

