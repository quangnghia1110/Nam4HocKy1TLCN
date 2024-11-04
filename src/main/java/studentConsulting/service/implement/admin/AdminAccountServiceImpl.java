package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.AccountEntity;
import studentConsulting.model.entity.DepartmentEntity;
import studentConsulting.model.entity.RoleConsultantEntity;
import studentConsulting.model.entity.RoleEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.manage.ManageAccountDTO;
import studentConsulting.model.payload.mapper.admin.AccountMapper;
import studentConsulting.repository.admin.AccountRepository;
import studentConsulting.repository.admin.DepartmentRepository;
import studentConsulting.repository.admin.RoleConsultantRepository;
import studentConsulting.repository.admin.RoleRepository;
import studentConsulting.service.interfaces.admin.IAdminAccountService;
import studentConsulting.specification.admin.AccountSpecification;

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

    @Autowired
    private AccountMapper accountMapper;

    @Override
    public Page<ManageAccountDTO> getAllAccountsWithFilters(String email, String username, Boolean isOnline, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Boolean isActivity, Pageable pageable) {
        Specification<AccountEntity> spec = Specification.where(null);

        if (email != null && !email.isEmpty()) {
            spec = spec.and(AccountSpecification.hasEmail(email));
        }

        if (username != null && !username.isEmpty()) {
            spec = spec.and(AccountSpecification.hasUsername(username));
        }

        if (isOnline != null) {
            spec = spec.and(AccountSpecification.isOnline(isOnline));
        }

        if (isActivity != null) {
            spec = spec.and(AccountSpecification.isActive(isActivity));
        }

        if (startDate.isPresent() && endDate.isPresent()) {
            spec = spec.and(AccountSpecification.hasExactDateRange(startDate.get(), endDate.get()));
        } else if (startDate.isPresent()) {
            spec = spec.and(AccountSpecification.hasExactStartDate(startDate.get()));
        } else if (endDate.isPresent()) {
            spec = spec.and(AccountSpecification.hasDateBefore(endDate.get()));
        }

        Page<AccountEntity> accountEntities = accountRepository.findAll(spec, pageable);
        return accountEntities.map(accountMapper::mapToDTO);
    }


    @Override
    public ManageAccountDTO getAccountById(Integer id) {
        AccountEntity accountEntity = accountRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tài khoản với ID: " + id));
        return accountMapper.mapToDTO(accountEntity);
    }

    @Override
    public ManageAccountDTO changeAccountActivity(Integer id) {
        AccountEntity accountEntity = accountRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tài khoản với ID: " + id));

        accountEntity.setActivity(!accountEntity.isActivity());
        AccountEntity updatedAccount = accountRepository.save(accountEntity);

        return accountMapper.mapToDTO(updatedAccount);
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
