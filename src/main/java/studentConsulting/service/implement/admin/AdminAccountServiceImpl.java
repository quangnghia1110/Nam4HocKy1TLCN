package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.entity.authentication.RoleEntity;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.user.RoleConsultantEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.authentication.ManageAccountDTO;
import studentConsulting.repository.authentication.AccountRepository;
import studentConsulting.repository.authentication.RoleRepository;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.user.RoleConsultantRepository;
import studentConsulting.service.interfaces.admin.IAdminAccountService;
import studentConsulting.specification.authentication.AccountSpecification;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdminAccountServiceImpl implements IAdminAccountService {

    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private RoleConsultantRepository roleConsultantRepository;

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
                .email(accountEntity.getEmail())
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

    @Override
    @Transactional
    public void importAccounts(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageAccountDTO> accounts = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String username = row.get(1);
                        String email = row.get(2);
                        Boolean isActivity = Boolean.parseBoolean(row.get(3));
                        Boolean isOnline = Boolean.parseBoolean(row.get(4));
                        LocalDate createdAt = LocalDate.parse(row.get(5));
                        LocalDateTime lastActivity = LocalDateTime.parse(row.get(6));
                        String departmentName = row.get(7);
                        String roleName = row.get(8);
                        String roleConsultantName = row.get(9);

                        DepartmentEntity department = departmentRepository.findByName(departmentName)
                                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với tên: " + departmentName));

                        RoleEntity role = roleRepository.findByNames(roleName)
                                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với tên: " + roleName));

                        RoleConsultantEntity roleConsultant = roleConsultantRepository.findByName(roleConsultantName)
                                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò tư vấn với tên: " + roleConsultantName));

                        return ManageAccountDTO.builder()
                                .id(id)
                                .username(username)
                                .email(email)
                                .createdAt(createdAt)
                                .isActivity(isActivity)
                                .isOnline(isOnline)
                                .lastActivity(lastActivity)
                                .department(ManageAccountDTO.DepartmentDTO.builder()
                                        .id(department.getId())
                                        .name(department.getName())
                                        .build())
                                .role(ManageAccountDTO.RoleDTO.builder()
                                        .id(role.getId())
                                        .name(role.getName())
                                        .build())
                                .roleConsultant(ManageAccountDTO.RoleConsultantDTO.builder()
                                        .id(roleConsultant.getId())
                                        .name(roleConsultant.getName())
                                        .build())
                                .build();
                    } catch (Exception e) {
                        throw new ErrorException("Lỗi khi phân tích dữ liệu tài khoản: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        accounts.forEach(accountDTO -> {
            try {
                AccountEntity accountEntity = new AccountEntity();
                accountEntity.setId(accountDTO.getId());
                accountEntity.setUsername(accountDTO.getUsername());
                accountEntity.setEmail(accountDTO.getEmail());
                accountEntity.setCreatedAt(accountDTO.getCreatedAt());
                accountEntity.setActivity(accountDTO.getIsActivity());
                accountEntity.setIsOnline(accountDTO.getIsOnline());
                accountEntity.setLastActivity(accountDTO.getLastActivity());

                DepartmentEntity department = departmentRepository.findById(accountDTO.getDepartment().getId())
                        .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với ID: " + accountDTO.getDepartment().getId()));

                RoleEntity role = roleRepository.findById(accountDTO.getRole().getId())
                        .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + accountDTO.getRole().getId()));

                RoleConsultantEntity roleConsultant = roleConsultantRepository.findById(accountDTO.getRoleConsultant().getId())
                        .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò tư vấn với ID: " + accountDTO.getRoleConsultant().getId()));

                accountEntity.setDepartment(department);
                accountEntity.setRole(role);
                accountEntity.setRoleConsultant(roleConsultant);

                accountRepository.save(accountEntity);
            } catch (Exception e) {
                throw new ErrorException("Lỗi khi lưu tài khoản vào cơ sở dữ liệu: " + e.getMessage());
            }
        });
    }


}
