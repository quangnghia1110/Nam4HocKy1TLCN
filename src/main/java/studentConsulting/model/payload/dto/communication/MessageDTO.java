package studentConsulting.model.payload.dto.communication;

import java.time.LocalDate;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.constant.enums.MessageStatus;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MessageDTO {
    private Integer id;
    private Integer conversationId;
    private UserInformationDTO sender;
    private List<UserInformationDTO> receiver;
    private String message;
    private String imageUrl;
    private LocalDate date;
    private MessageStatus messageStatus;
    
    @Data
	@Builder
	public static class UserInformationDTO {
		private Integer id;
		private String avatarUrl;
		private String name;
	}
    
    public void setReceivers(List<UserInformationDTO> receivers) {
        this.receiver = receivers;
    }

}
