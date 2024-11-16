package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.manage.ManageUserDTO;

import java.time.LocalDate;
import java.util.Optional;

public interface IAdminUserInformationService {
    Page<ManageUserDTO> getUserByAdmin(Integer accountId, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Pageable pageable);

    ManageUserDTO getUserById(Integer id);

    public ManageUserDTO updateUserInformation(Integer id, ManageUserDTO userRequest);

    }
