package studentConsulting.model.payload.dto.user;

import java.time.LocalDate;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ManageUserInformationDTO {
	private UserDTO user;
	private AccountDTO account;
	private AddressDTO address;
	private DepartmentDTO department;
	private LocalDate createdAt;
	
	@Data
	@Builder
	public static class UserDTO {
		private Integer id;
		private String schoolName;
		private String studentCode;
		private String firstName;
		private String lastName;
		private String phone;
		private String avatarUrl;
		private String gender;
	}

	@Data
	@Builder
	public static class AccountDTO {
		private String email;
	    private String username;
	}

	@Data
	@Builder
	public static class AddressDTO {
		private String line;
		private ProvinceDTO province;
		private DistrictDTO district;
		private WardDTO ward;
	}

	@Data
	@Builder
	public static class ProvinceDTO {
		private String code;
		private String fullName;
	}

	@Data
	@Builder
	public static class DistrictDTO {
		private String code;
		private String fullName;
	}

	@Data
	@Builder
	public static class WardDTO {
		private String id;
		private String fullName;
	}

	@Data
	@Builder
	public static class DepartmentDTO {
		private Integer id;
		private String name;
	}
}
