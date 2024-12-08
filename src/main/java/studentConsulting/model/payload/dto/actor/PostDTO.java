package studentConsulting.model.payload.dto.actor;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PostDTO {
    private Integer id;
	private String title;
    private String content;
    private Integer userId;
    private boolean isAnonymous;
    private LocalDate createdAt;
    private String name;
    private String avatarUrl;
    private String fileName; 
    private boolean isApproved;
    private int views;
    private int totalComments;
}

