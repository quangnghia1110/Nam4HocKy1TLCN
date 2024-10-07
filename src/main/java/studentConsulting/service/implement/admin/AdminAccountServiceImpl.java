package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.entity.authentication.RoleEntity;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.user.RoleConsultantEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.authentication.ManageAccountDTO;
import studentConsulting.repository.authentication.AccountRepository;
import studentConsulting.service.interfaces.admin.IAdminAccountService;
import studentConsulting.specification.authentication.AccountSpecification;

import java.time.LocalDate;
import java.util.Optional;

@Service
public class AdminAccountServiceImpl implements IAdminAccountService {

    @Autowired
    private AccountRepository accountRepository;

    @Override
    public Page<ManageAccountDTO> getAllAccountsWithFilters(Optional<String> email, Optional<String> username, Optional<Boolean> isOnline, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Optional<Boolean> isActivity, Pageable pageable) {
        Specification<AccountEntity> spec = Specification.where(null);

        if (email.isPresent()) {
            spec = spec.and(AccountSpecification.hasEmail(email.get()));
        }

        if (username.isPresent()) {
            spec = spec.and(AccountSpecification.hasUsername(username.get()));
        }

        if (isOnline.isPresent()) {
            spec = spec.and(AccountSpecification.isOnline(isOnline.get()));
        }

        if (isActivity.isPresent()) {
            spec = spec.and(AccountSpecification.isActive(isActivity.get()));
        }

        if (startDate.isPresent() && endDate.isPresent()) {
            spec = spec.and(AccountSpecification.hasExactDateRange(startDate.get(), endDate.get()));
        } else if (startDate.isPresent()) {
            spec = spec.and(AccountSpecification.hasExactStartDate(startDate.get()));
        } else if (endDate.isPresent()) {
            spec = spec.and(AccountSpecification.hasDateBefore(endDate.get()));
        }

        Page<AccountEntity> accountEntities = accountRepository.findAll(spec, pageable);
        return accountEntities.map(this::mapToDTO);
    }


    @Override
    public ManageAccountDTO getAccountById(Integer id) {
        AccountEntity accountEntity = accountRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tài khoản với ID: " + id));
        return mapToDTO(accountEntity);
    }

    @Override
    public ManageAccountDTO changeAccountActivity(Integer id) {
        AccountEntity accountEntity = accountRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tài khoản với ID: " + id));

        accountEntity.setActivity(!accountEntity.isActivity());
        AccountEntity updatedAccount = accountRepository.save(accountEntity);

        return mapToDTO(updatedAccount);
    }

    private ManageAccountDTO mapToDTO(AccountEntity accountEntity) {
        return ManageAccountDTO.builder()
                .id(accountEntity.getId())
                .createdAt(accountEntity.getCreatedAt())
                .isActivity(accountEntity.isActivity())
                .username(accountEntity.getUsername())
                .department(accountEntity.getDepartment() != null ? mapToDepartmentDTO(accountEntity.getDepartment()) : null)
                .role(accountEntity.getRole() != null ? mapToRoleDTO(accountEntity.getRole()) : null)
                .roleConsultant(accountEntity.getRoleConsultant() != null ? mapToRoleConsultantDTO(accountEntity.getRoleConsultant()) : null)
                .lastActivity(accountEntity.getLastActivity())
                .isOnline(accountEntity.getIsOnline())
                .build();
    }

    private ManageAccountDTO.DepartmentDTO mapToDepartmentDTO(DepartmentEntity departmentEntity) {
        return ManageAccountDTO.DepartmentDTO.builder()
                .id(departmentEntity.getId())
                .name(departmentEntity.getName())
                .build();
    }

    private ManageAccountDTO.RoleDTO mapToRoleDTO(RoleEntity roleEntity) {
        return ManageAccountDTO.RoleDTO.builder()
                .id(roleEntity.getId())
                .name(roleEntity.getName())
                .build();
    }

    private ManageAccountDTO.RoleConsultantDTO mapToRoleConsultantDTO(RoleConsultantEntity roleConsultantEntity) {
        return ManageAccountDTO.RoleConsultantDTO.builder()
                .id(roleConsultantEntity.getId())
                .name(roleConsultantEntity.getName())
                .build();
    }
}
