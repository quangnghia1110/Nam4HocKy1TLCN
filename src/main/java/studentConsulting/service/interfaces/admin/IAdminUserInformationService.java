package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.user.ManageUserDTO;

import java.time.LocalDate;
import java.util.Optional;

public interface IAdminUserInformationService {
    Page<ManageUserDTO> getAllUsersWithFilters(Optional<String> name, Optional<String> studentCode, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Pageable pageable);

    ManageUserDTO getUserById(Integer id);
}
