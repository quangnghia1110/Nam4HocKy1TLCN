package studentConsulting.model.payload.request.answer;

import org.springframework.web.multipart.MultipartFile;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CreateAnswerRequest {
	private Integer questionId;          
    private Integer roleConsultantId;   
    private Integer consultantId;              
    private String title;                
    private String content;              
    private MultipartFile file; 
    private Boolean statusApproval;
}
