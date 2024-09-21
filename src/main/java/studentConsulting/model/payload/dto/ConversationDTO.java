package studentConsulting.model.payload.dto;

import java.time.LocalDate;
import java.util.List;

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
	private DepartmentDTO department;
	private String userName;
	private UserInformationDTO consultant;
	private String name;
	private Boolean isGroup;
	private LocalDate createdAt;
	private List<MemberDTO> members;

	@Data
	@Builder
	public static class UserInformationDTO {
		private String avatarUrl;
		private String consultantName;
	}
}
