package studentConsulting.model.payload.dto.common;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class StatisticAdminDTO {
    private Long totalAccounts;
    private Long totalAddresses;
    private Long totalDepartments;
    private Long totalDistricts;
    private Long totalFields;
    private Long totalProvinces;
    private Long totalRoleAsks;
    private Long totalRoleConsultants;
    private Long totalRoles;
    private Long totalWards;
}

