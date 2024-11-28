package studentConsulting.service.implement.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.*;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.*;
import studentConsulting.model.payload.dto.manage.*;
import studentConsulting.model.payload.mapper.actor.*;
import studentConsulting.model.payload.mapper.admin.*;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.actor.*;
import studentConsulting.repository.admin.*;
import studentConsulting.service.interfaces.actor.*;
import studentConsulting.service.interfaces.admin.*;
import studentConsulting.service.interfaces.common.IExportImportService;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
public class ExportImportServiceImpl implements IExportImportService {

    @Autowired
    private RoleConsultantRepository roleConsultantRepository;
    @Autowired
    private PostRepository postRepository;
    @Autowired
    private UserRepository consultantRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private RoleRepository roleRepository;
    @Autowired
    private AnswerRepository answerRepository;
    @Autowired
    private CommonQuestionRepository commonQuestionRepository;
    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;
    @Autowired
    private ConversationRepository conversationRepository;
    @Autowired
    private ForwardQuestionRepository forwardQuestionRepository;
    @Autowired
    private QuestionRepository questionRepository;
    @Autowired
    private AccountRepository accountRepository;
    @Autowired
    private AddressRepository addressRepository;
    @Autowired
    private DepartmentRepository departmentRepository;
    @Autowired
    private DistrictRepository districtRepository;
    @Autowired
    private FieldRepository fieldRepository;
    @Autowired
    private ProvinceRepository provinceRepository;
    @Autowired
    private RoleAskRepository roleAskRepository;
    @Autowired
    private UserRepository userInformationRepository;
    @Autowired
    private WardRepository wardRepository;


    @Autowired
    private IPostService postService;

    @Autowired
    private IAnswerService answerService;

    @Autowired
    private ICommonQuestionService commonQuestionService;

    @Autowired
    private IConsultationScheduleService consultationScheduleService;

    @Autowired
    private IConversationService conversationService;

    @Autowired
    private IForwardQuestionService forwardQuestionService;

    @Autowired
    private IQuestionService questionService;

    @Autowired
    private IAdminAccountService accountService;

    @Autowired
    private IAdminAdressService addressService;

    @Autowired
    private IAdminDepartmentService departmentService;

    @Autowired
    private IAdminDistrictService districtService;

    @Autowired
    private IAdminFieldService fieldService;

    @Autowired
    private IAdminProvinceService provinceService;

    @Autowired
    private IAdminRoleAskService roleAskService;

    @Autowired
    private IAdminRoleConsultantService roleConsultantService;

    @Autowired
    private IAdminRoleService roleService;

    @Autowired
    private IAdminUserInformationService userInformationService;

    @Autowired
    private IAdminWardService wardService;

    @Autowired
    private AnswerMapper answerMapper;
    @Autowired
    private CommonQuestionMapper commonQuestionMapper;
    @Autowired
    private ConsultationScheduleMapper consultationScheduleMapper;
    @Autowired
    private ConversationMapper conversationMapper;
    @Autowired
    private ForwardQuestionMapper forwardQuestionMapper;
    @Autowired
    private QuestionMapper questionMapper;
    @Autowired
    private AccountMapper accountMapper;
    @Autowired
    private AddressMapper addressMapper;
    @Autowired
    private DepartmentMapper departmentMapper;
    @Autowired
    private DistrictMapper districtMapper;
    @Autowired
    private FieldMapper fieldMapper;
    @Autowired
    private ProvinceMapper provinceMapper;
    @Autowired
    private RoleAskMapper roleAskMapper;
    @Autowired
    private RoleConsultantMapper roleConsultantMapper;
    @Autowired
    private UserInformationMapper userInformationMapper;
    @Autowired
    private WardMapper wardMapper;

    @Override
    public void importCommonQuestions(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<CommonQuestionDTO> commonQuestions = filteredData.stream()
                .map(row -> {
                    try {
                        String departmentName = row.get(0);
                        String fieldName = row.get(1);
                        String roleAskName = row.get(2);
                        String title = row.get(3);
                        String content = row.get(4);
                        String fileName = row.get(5);
                        Integer views = Integer.parseInt(row.get(6));
                        String askerFirstname = row.get(7);
                        String askerLastname = row.get(8);
                        String answerTitle = row.get(9);
                        String answerContent = row.get(10);
                        String answerUserEmail = row.get(11);
                        String answerUserFirstname = row.get(12);
                        String answerUserLastname = row.get(13);
                        LocalDate answerCreatedAt = LocalDate.parse(row.get(14));
                        String createdBy = row.get(15);

                        var department = departmentRepository.findByName(departmentName)
                                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với tên: " + departmentName));

                        var field = fieldRepository.findByName(fieldName)
                                .orElseThrow(() -> new ErrorException("Không tìm thấy lĩnh vực với tên: " + fieldName));

                        var roleAsk = roleAskRepository.findByName(roleAskName)
                                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với tên: " + roleAskName));

                        return CommonQuestionDTO.builder()
                                .department(CommonQuestionDTO.DepartmentDTO.builder()
                                        .id(department.getId())
                                        .name(departmentName)
                                        .build())
                                .field(CommonQuestionDTO.FieldDTO.builder()
                                        .id(field.getId())
                                        .name(fieldName)
                                        .build())
                                .roleAsk(CommonQuestionDTO.RoleAskDTO.builder()
                                        .id(roleAsk.getId())
                                        .name(roleAskName)
                                        .build())
                                .title(title)
                                .content(content)
                                .fileName(fileName)
                                .views(views)
                                .askerFirstname(askerFirstname)
                                .askerLastname(askerLastname)
                                .answerTitle(answerTitle)
                                .answerContent(answerContent)
                                .answerUserEmail(answerUserEmail)
                                .answerUserFirstname(answerUserFirstname)
                                .answerUserLastname(answerUserLastname)
                                .answerCreatedAt(answerCreatedAt)
                                .createdBy(createdBy)
                                .build();
                    } catch (Exception e) {
                        throw new ErrorException("Lỗi khi parse dữ liệu Common Question: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        commonQuestions.forEach(question -> {
            try {
                CommonQuestionEntity entity = new CommonQuestionEntity();
                entity.setDepartment(departmentRepository.findByName(question.getDepartment().getName()).get());
                entity.setField(fieldRepository.findByName(question.getField().getName()).get());
                entity.setRoleAsk(roleAskRepository.findByName(question.getRoleAsk().getName()).get());
                entity.setTitle(question.getTitle());
                entity.setContent(question.getContent());
                entity.setFileName(question.getFileName());
                entity.setViews(question.getViews());
                entity.setAskerFirstname(question.getAskerFirstname());
                entity.setAskerLastname(question.getAskerLastname());
                entity.setAnswerTitle(question.getAnswerTitle());
                entity.setAnswerContent(question.getAnswerContent());
                entity.setAnswerUserEmail(question.getAnswerUserEmail());
                entity.setAnswerUserFirstname(question.getAnswerUserFirstname());
                entity.setAnswerUserLastname(question.getAnswerUserLastname());
                entity.setAnswerCreatedAt(question.getAnswerCreatedAt());
                String id = question.getCreatedBy();

                UserInformationEntity createdBy = userRepository.findById(Integer.parseInt(id))
                        .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

                entity.setCreatedBy(createdBy);


                commonQuestionRepository.save(entity);
            } catch (Exception e) {
                throw new ErrorException("Lỗi khi lưu Common Question vào database: " + e.getMessage());
            }
        });
    }

    @Override
    public void importManageConsultantSchedules(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream().skip(1).collect(Collectors.toList());

        List<ManageConsultantScheduleDTO> consultantSchedules = filteredData.stream().map(row -> {
            try {
                Integer id = Integer.parseInt(row.get(0));
                String title = row.get(1);
                String content = row.get(2);
                LocalDate consultationDate = LocalDate.parse(row.get(3));
                String consultationTime = row.get(4);
                String location = row.get(5);
                String link = row.get(6);
                Boolean mode = Boolean.parseBoolean(row.get(7));
                Boolean statusPublic = Boolean.parseBoolean(row.get(8));
                Boolean statusConfirmed = Boolean.parseBoolean(row.get(9));
                Integer createdById = Integer.parseInt(row.get(10));

                return ManageConsultantScheduleDTO.builder()
                        .id(id)
                        .title(title)
                        .content(content)
                        .consultationDate(consultationDate)
                        .consultationTime(consultationTime)
                        .location(location)
                        .link(link)
                        .mode(mode)
                        .statusPublic(statusPublic)
                        .statusConfirmed(statusConfirmed)
                        .created_by(createdById)
                        .build();
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Consultation Schedule: " + e.getMessage());
            }
        }).collect(Collectors.toList());

        consultantSchedules.forEach(schedule -> {
            try {
                ConsultationScheduleEntity entity = new ConsultationScheduleEntity();
                entity.setId(schedule.getId());
                entity.setTitle(schedule.getTitle());
                entity.setContent(schedule.getContent());
                entity.setConsultationDate(schedule.getConsultationDate());
                entity.setConsultationTime(schedule.getConsultationTime());
                entity.setLocation(schedule.getLocation());
                entity.setLink(schedule.getLink());
                entity.setMode(schedule.getMode());
                entity.setStatusPublic(schedule.getStatusPublic());
                entity.setStatusConfirmed(schedule.getStatusConfirmed());

                UserInformationEntity createdBy = userRepository.findById(schedule.getCreated_by())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy người tạo với ID: " + schedule.getCreated_by()));

                entity.setCreatedBy(createdBy.getId());

                consultationScheduleRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Consultation Schedule vào database: " + e.getMessage());
            }
        });
    }

    @Override
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

    @Override
    public void importAddresses(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageAddressDTO> addresses = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String line = row.get(1);
                        String provinceCode = row.get(2);
                        String districtCode = row.get(3);
                        String wardCode = row.get(4);

                        return new ManageAddressDTO(id, line, provinceCode, districtCode, wardCode);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Address: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        addresses.forEach(address -> {
            try {
                AddressEntity entity = new AddressEntity();
                entity.setId(address.getId());
                entity.setLine(address.getLine());

                ProvinceEntity province = provinceRepository.findByCode(address.getProvinceCode())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy tỉnh với mã: " + address.getProvinceCode()));
                DistrictEntity district = districtRepository.findByCode(address.getDistrictCode())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy huyện với mã: " + address.getDistrictCode()));
                WardEntity ward = wardRepository.findByCode(address.getWardCode())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy xã với mã: " + address.getWardCode()));

                entity.setProvince(province);
                entity.setDistrict(district);
                entity.setWard(ward);

                addressRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Address vào database: " + e.getMessage());
            }
        });
    }

    @Override
    public void importDepartments(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        List<ManageDepartmentDTO> departments = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        LocalDate createdAt = LocalDate.parse(row.get(1), formatter);  // Chuyển đổi thành LocalDate
                        String description = row.get(2);
                        String logo = row.get(3);
                        String name = row.get(4);

                        return new ManageDepartmentDTO(id, createdAt, description, logo, name);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Department: ");
                    }
                })
                .collect(Collectors.toList());

        departments.forEach(department -> {
            try {
                DepartmentEntity entity = new DepartmentEntity();
                entity.setId(department.getId());
                entity.setCreatedAt(department.getCreatedAt());
                entity.setDescription(department.getDescription());
                entity.setLogo(department.getLogo());
                entity.setName(department.getName());

                departmentRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Department vào database: ");
            }
        });
    }

    @Override
    public void importDistricts(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageDistrictDTO> districts = filteredData.stream()
                .map(row -> {
                    try {
                        String code = row.get(0);
                        String name = row.get(1);
                        String nameEn = row.get(2);
                        String fullName = row.get(3);
                        String fullNameEn = row.get(4);
                        String codeName = row.get(5);
                        String provinceCode = row.get(6);

                        return new ManageDistrictDTO(code, name, nameEn, fullName, fullNameEn, codeName, provinceCode);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu District: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        districts.forEach(district -> {
            try {
                DistrictEntity entity = new DistrictEntity();
                entity.setCode(district.getCode());
                entity.setName(district.getName());
                entity.setNameEn(district.getNameEn());
                entity.setFullName(district.getFullName());
                entity.setFullNameEn(district.getFullNameEn());
                entity.setCodeName(district.getCodeName());

                ProvinceEntity province = provinceRepository.findByCode(district.getProvinceCode())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy tỉnh với mã: " + district.getProvinceCode()));
                entity.setProvince(province);

                districtRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu District vào database: " + e.getMessage());
            }
        });
    }

    @Override
    public void importFields(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        List<ManageFieldDTO> fields = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        LocalDate createdAt = LocalDate.parse(row.get(1), formatter);
                        String name = row.get(2);
                        Integer departmentId = Integer.parseInt(row.get(3));

                        return new ManageFieldDTO(id, createdAt, name, departmentId);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Field");
                    }
                })
                .collect(Collectors.toList());

        fields.forEach(field -> {
            try {
                FieldEntity entity = new FieldEntity();
                entity.setId(field.getId());
                entity.setCreatedAt(field.getCreatedAt());
                entity.setName(field.getName());

                DepartmentEntity department = departmentRepository.findById(field.getDepartmentId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Department ID không hợp lệ"));

                entity.setDepartment(department);

                fieldRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Field vào database");
            }
        });
    }

    @Override
    public void importProvinces(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageProvinceDTO> provinces = filteredData.stream()
                .map(row -> {
                    try {
                        String code = row.get(0);
                        String name = row.get(1);
                        String nameEn = row.get(2);
                        String fullName = row.get(3);
                        String fullNameEn = row.get(4);
                        String codeName = row.get(5);

                        return new ManageProvinceDTO(code, name, nameEn, fullName, fullNameEn, codeName);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Province: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        provinces.forEach(province -> {
            try {
                ProvinceEntity entity = new ProvinceEntity();
                entity.setCode(province.getCode());
                entity.setName(province.getName());
                entity.setNameEn(province.getNameEn());
                entity.setFullName(province.getFullName());
                entity.setFullNameEn(province.getFullNameEn());
                entity.setCodeName(province.getCodeName());

                provinceRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Province vào database: " + e.getMessage());
            }
        });
    }

    @Override
    public void importRoleAsks(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        List<ManageRoleAskDTO> roleAsks = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String name = row.get(1);
                        LocalDate createdAt = LocalDate.parse(row.get(2), dateFormatter);
                        Integer roleId = Integer.parseInt(row.get(3));

                        return ManageRoleAskDTO.builder()
                                .id(id)
                                .name(name)
                                .createdAt(createdAt)
                                .roleId(roleId)
                                .build();
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Role Ask: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        roleAsks.forEach(roleAsk -> {
            try {
                RoleAskEntity entity = new RoleAskEntity();
                entity.setId(roleAsk.getId());
                entity.setName(roleAsk.getName());
                entity.setCreatedAt(roleAsk.getCreatedAt());

                RoleEntity role = roleRepository.findById(roleAsk.getRoleId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy vai trò với ID: " + roleAsk.getRoleId()));
                entity.setRole(role);

                roleAskRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Role Ask vào database: " + e.getMessage());
            }
        });
    }

    @Override
    public void importRoleConsultants(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageRoleConsultantDTO> roleConsultants = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String name = row.get(1);
                        Integer roleId = Integer.parseInt(row.get(2));
                        LocalDate createdAt = LocalDate.parse(row.get(3));

                        return ManageRoleConsultantDTO.builder()
                                .id(id)
                                .name(name)
                                .roleId(roleId)
                                .createdAt(createdAt)
                                .build();
                    } catch (Exception e) {
                        throw new ErrorException("Lỗi khi parse dữ liệu Role Consultant: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        roleConsultants.forEach(roleConsultantDTO -> {
            try {
                RoleConsultantEntity entity = new RoleConsultantEntity();
                entity.setId(roleConsultantDTO.getId());
                entity.setName(roleConsultantDTO.getName());
                entity.setCreatedAt(roleConsultantDTO.getCreatedAt());

                RoleEntity role = roleRepository.findById(roleConsultantDTO.getRoleId())
                        .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + roleConsultantDTO.getRoleId()));
                entity.setRole(role);

                roleConsultantRepository.save(entity);
            } catch (Exception e) {
                throw new ErrorException("Lỗi khi lưu Role Consultant vào database: " + e.getMessage());
            }
        });
    }

    @Override
    public void importRoles(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<RoleDTO> roles = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String name = row.get(1);
                        LocalDate createdAt = LocalDate.parse(row.get(3));
                        return new RoleDTO(id, name, createdAt);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Role: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        roles.forEach(role -> {
            try {
                RoleEntity entity = new RoleEntity();
                entity.setId(role.getId());
                entity.setName(role.getName());
                entity.setCreatedAt(role.getCreatedAt());
                roleRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Role vào database: " + e.getMessage());
            }
        });
    }

    @Override
    public void importUsers(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageUserDTO> users = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String firstName = row.get(1);
                        String lastName = row.get(2);
                        String studentCode = row.get(3);
                        String gender = row.get(4);
                        String phone = row.get(5);
                        String schoolName = row.get(6);
                        LocalDate createdAt = LocalDate.parse(row.get(7));
                        String line = row.get(8);
                        String province = row.get(9);
                        String district = row.get(10);
                        String ward = row.get(11);

                        ManageUserDTO.AddressDTO addressDTO = ManageUserDTO.AddressDTO.builder()
                                .line(line)
                                .provinceFullName(province)
                                .districtFullName(district)
                                .wardFullName(ward)
                                .build();

                        return ManageUserDTO.builder()
                                .id(id)
                                .firstName(firstName)
                                .lastName(lastName)
                                .studentCode(studentCode)
                                .gender(gender)
                                .phone(phone)
                                .schoolName(schoolName)
                                .createdAt(createdAt)
                                .address(addressDTO)
                                .build();
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu người dùng: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        users.forEach(user -> {
            try {
                UserInformationEntity entity = new UserInformationEntity();
                entity.setId(user.getId());
                entity.setFirstName(user.getFirstName());
                entity.setLastName(user.getLastName());
                entity.setStudentCode(user.getStudentCode());
                entity.setGender(user.getGender());
                entity.setPhone(user.getPhone());
                entity.setSchoolName(user.getSchoolName());
                entity.setCreatedAt(user.getCreatedAt());

                AddressEntity addressEntity = new AddressEntity();
                addressEntity.setLine(user.getAddress().getLine());

                ProvinceEntity provinceEntity = provinceRepository.findByFullName(user.getAddress().getProvinceFullName())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy tỉnh với tên: " + user.getAddress().getProvinceFullName()));

                DistrictEntity districtEntity = districtRepository.findByFullName(user.getAddress().getDistrictFullName())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy quận với tên: " + user.getAddress().getDistrictFullName()));

                WardEntity wardEntity = wardRepository.findByFullName(user.getAddress().getWardFullName())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy phường với tên: " + user.getAddress().getWardFullName()));

                addressEntity.setProvince(provinceEntity);
                addressEntity.setDistrict(districtEntity);
                addressEntity.setWard(wardEntity);

                entity.setAddress(addressEntity);

                userInformationRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu người dùng vào cơ sở dữ liệu: " + e.getMessage());
            }
        });
    }

    @Override
    public void importWards(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageWardDTO> wards = filteredData.stream()
                .map(row -> {
                    try {
                        String code = row.get(0);
                        String name = row.get(1);
                        String nameEn = row.get(2);
                        String fullName = row.get(3);
                        String fullNameEn = row.get(4);
                        String codeName = row.get(5);
                        String districtCode = row.get(6);

                        return new ManageWardDTO(code, name, nameEn, fullName, fullNameEn, codeName, districtCode);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Ward: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        wards.forEach(ward -> {
            try {
                WardEntity entity = new WardEntity();
                entity.setCode(ward.getCode());
                entity.setName(ward.getName());
                entity.setNameEn(ward.getNameEn());
                entity.setFullName(ward.getFullName());
                entity.setFullNameEn(ward.getFullNameEn());
                entity.setCodeName(ward.getCodeName());

                DistrictEntity district = districtRepository.findByCode(ward.getDistrictCode())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy quận/huyện với mã: " + ward.getDistrictCode()));
                entity.setDistrict(district);

                wardRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Ward vào database: " + e.getMessage());
            }
        });
    }

    @Override
    public String getStringValue(Object obj) {
        return obj != null ? obj.toString() : "Không có";
    }

    @Override
    public ResponseEntity<DataResponse> buildResponse(String message) {
        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message(message)
                .build());
    }

    @Override
    public String buildHeaderByPdf(Object item) {
        StringBuilder headers = new StringBuilder("<tr>");

        if (item instanceof CommonQuestionDTO) {
            headers.append("<th>Mã câu hỏi chung</th>")
                    .append("<th>Tên phòng ban</th>")
                    .append("<th>Tên lĩnh vực</th>")
                    .append("<th>Vai trò hỏi</th>")
                    .append("<th>Tên người hỏi</th>")
                    .append("<th>Tiêu đề</th>")
                    .append("<th>Nội dung</th>")
                    .append("<th>Tên người trả lời</th>")
                    .append("<th>Email người trả lời</th>")
                    .append("<th>Tiêu đề câu trả lời</th>")
                    .append("<th>Nội dung câu trả lời</th>");
        } else if (item instanceof ConsultationScheduleDTO) {
            headers.append("<th>Mã lịch</th>")
                    .append("<th>Tên phòng ban</th>")
                    .append("<th>Tiêu đề</th>")
                    .append("<th>Nội dung</th>")
                    .append("<th>Tên người tư vấn</th>")
                    .append("<th>Ngày tư vấn</th>")
                    .append("<th>Giờ tư vấn</th>")
                    .append("<th>Địa điểm</th>")
                    .append("<th>Link</th>")
                    .append("<th>Trạng thái online</th>")
                    .append("<th>Trạng thái công khai</th>")
                    .append("<th>Trạng thái xác nhận</th>");
        } else if (item instanceof ConversationDTO) {
            headers.append("<th>Mã cuộc hội thoại</th>")
                    .append("<th>Tên cuộc hội thoại</th>")
                    .append("<th>Tên phòng ban</th>")
                    .append("<th>Ngày tạo</th>")
                    .append("<th>Trạng thái nhóm</th>");
        } else if (item instanceof ForwardQuestionDTO) {
            headers.append("<th>Mã câu hỏi chuyển tiếp</th>")
                    .append("<th>Tên phòng ban gửi</th>")
                    .append("<th>Tên phòng ban nhận</th>")
                    .append("<th>Tên người tư vấn</th>");
        } else if (item instanceof MyQuestionDTO) {
            headers.append("<th>Mã câu hỏi</th>")
                    .append("<th>Tên phòng ban</th>")
                    .append("<th>Tên lĩnh vực</th>")
                    .append("<th>Tên người hỏi</th>")
                    .append("<th>Tiêu đề</th>")
                    .append("<th>Nội dung</th>")
                    .append("<th>Tên người trả lời</th>")
                    .append("<th>Tiêu đề trả lời</th>")
                    .append("<th>Nội dung trả lời</th>");
        } else if (item instanceof PostDTO) {
            headers.append("<th>Mã câu hỏi</th>")
                    .append("<th>Tiêu đề</th>")
                    .append("<th>Nội dung</th>")
                    .append("<th>Tên người dùng</th>")
                    .append("<th>Ảnh đại diện</th>")
                    .append("<th>Tên tệp đính kèm</th>")
                    .append("<th>Ngày tạo</th>")
                    .append("<th>Lượt xem</th>")
                    .append("<th>Ẩn danh</th>")
                    .append("<th>Đã duyệt</th>");
        } else if (item instanceof RatingDTO) {
            headers.append("<th>Mã đánh giá</th>")
                    .append("<th>Tên phòng ban</th>")
                    .append("<th>Tên người dùng</th>")
                    .append("<th>Tên tư vấn viên</th>")
                    .append("<th>Độ hài lòng chung</th>")
                    .append("<th>Nhận xét chung</th>")
                    .append("<th>Kiến thức chuyên môn</th>")
                    .append("<th>Nhận xét</th>")
                    .append("<th>Thái độ</th>")
                    .append("<th>Nhận xét</th>")
                    .append("<th>Tốc độ phản hồi</th>")
                    .append("<th>Nhận xét</th>")
                    .append("<th>Khả năng hiểu vấn đề</th>")
                    .append("<th>Nhận xét</th>")
                    .append("<th>Ngày gửi</th>");
        } else if (item instanceof ManageAccountDTO) {
            headers.append("<th>Mã tài khoản</th>")
                    .append("<th>Tên đăng nhập</th>")
                    .append("<th>Email</th>")
                    .append("<th>Ngày tạo</th>")
                    .append("<th>Tên phòng ban</th>")
                    .append("<th>Vai trò</th>")
                    .append("<th>Vai trò tư vấn</th>")
                    .append("<th>Hoạt động</th>");
        } else if (item instanceof ManageAddressDTO) {
            headers.append("<th>Mã địa chỉ</th>")
                    .append("<th>Tên đường</th>")
                    .append("<th>Mã tỉnh</th>")
                    .append("<th>Mã huyện</th>")
                    .append("<th>Mã xã</th>");
        } else if (item instanceof ManageDepartmentDTO) {
            headers.append("<th>Mã phòng ban</th>")
                    .append("<th>Tên phòng ban</th>")
                    .append("<th>Ngày tạo</th>")
                    .append("<th>Mô tả</th>")
                    .append("<th>Logo</th>");
        } else if (item instanceof ManageDistrictDTO) {
            headers.append("<th>Mã huyện</th>")
                    .append("<th>Tên huyện</th>")
                    .append("<th>Tên (tiếng Anh)</th>")
                    .append("<th>Tên đầy đủ</th>")
                    .append("<th>Tên đầy đủ (tiếng Anh)</th>")
                    .append("<th>Tên mã</th>")
                    .append("<th>Mã tỉnh</th>");
        } else if (item instanceof ManageFieldDTO) {
            headers.append("<th>Mã lĩnh vực</th>")
                    .append("<th>Tên lĩnh vực</th>")
                    .append("<th>Ngày tạo</th>")
                    .append("<th>Mã phòng ban</th>");
        } else if (item instanceof ManageProvinceDTO) {
            headers.append("<th>Mã tỉnh</th>")
                    .append("<th>Tên tỉnh</th>")
                    .append("<th>Tên (tiếng Anh)</th>")
                    .append("<th>Tên đầy đủ</th>")
                    .append("<th>Tên đầy đủ (tiếng Anh)</th>")
                    .append("<th>Tên mã</th>");
        } else if (item instanceof ManageRoleAskDTO) {
            headers.append("<th>Mã vai trò hỏi</th>")
                    .append("<th>Tên vai trò hỏi</th>")
                    .append("<th>Mã vai trò</th>")
                    .append("<th>Ngày tạo</th>");
        } else if (item instanceof ManageRoleConsultantDTO) {
            headers.append("<th>Mã vai trò tư vấn</th>")
                    .append("<th>Tên vai trò tư vấn</th>")
                    .append("<th>Mã vai trò</th>")
                    .append("<th>Ngày tạo</th>");
        } else if (item instanceof RoleDTO) {
            headers.append("<th>Mã vai trò</th>")
                    .append("<th>Tên vai trò</th>");
        } else if (item instanceof ManageUserDTO) {
            headers.append("<th>Mã người dùng</th>")
                    .append("<th>Họ tên</th>")
                    .append("<th>Mã sinh viên</th>")
                    .append("<th>Giới tính</th>")
                    .append("<th>Số điện thoại</th>")
                    .append("<th>Ngày tạo</th>")
                    .append("<th>Tên đường</th>")
                    .append("<th>Tên tỉnh</th>")
                    .append("<th>Tên huyện</th>")
                    .append("<th>Tên xã</th>");
        } else if (item instanceof ManageWardDTO) {
            headers.append("<th>Mã xã</th>")
                    .append("<th>Tên xã</th>")
                    .append("<th>Tên (tiếng Anh)</th>")
                    .append("<th>Tên đầy đủ</th>")
                    .append("<th>Tên đầy đủ (tiếng Anh)</th>")
                    .append("<th>Tên mã</th>")
                    .append("<th>Mã huyện</th>");
        }

        headers.append("</tr>");
        return headers.toString();
    }

    @Override
    public String buildDataByPdf(List<?> items) {
        StringBuilder dataRows = new StringBuilder();

        for (Object item : items) {
            dataRows.append("<tr>");

            if (item instanceof CommonQuestionDTO) {
                CommonQuestionDTO question = (CommonQuestionDTO) item;
                dataRows.append("<td>").append(getStringValue(question.getCommonQuestionId())).append("</td>")
                        .append("<td>").append(getStringValue(question.getDepartment().getName())).append("</td>")
                        .append("<td>").append(getStringValue(question.getField().getName())).append("</td>")
                        .append("<td>").append(getStringValue(question.getRoleAsk().getName())).append("</td>")
                        .append("<td>").append(getStringValue(question.getAskerLastname() + " " + question.getAskerFirstname())).append("</td>")
                        .append("<td>").append(getStringValue(question.getTitle())).append("</td>")
                        .append("<td>").append(getStringValue(question.getContent())).append("</td>")
                        .append("<td>").append(getStringValue(question.getAnswerUserLastname() + " " + question.getAnswerUserFirstname())).append("</td>")
                        .append("<td>").append(getStringValue(question.getAnswerUserEmail())).append("</td>")
                        .append("<td>").append(getStringValue(question.getAnswerTitle())).append("</td>")
                        .append("<td>").append(getStringValue(question.getAnswerContent())).append("</td>");
            } else if (item instanceof ConsultationScheduleDTO) {
                ConsultationScheduleDTO schedule = (ConsultationScheduleDTO) item;
                dataRows.append("<td>").append(getStringValue(schedule.getId())).append("</td>")
                        .append("<td>").append(getStringValue(schedule.getDepartment().getName())).append("</td>")
                        .append("<td>").append(getStringValue(schedule.getTitle())).append("</td>")
                        .append("<td>").append(getStringValue(schedule.getContent())).append("</td>")
                        .append("<td>").append(getStringValue(schedule.getConsultantName())).append("</td>")
                        .append("<td>").append(getStringValue(schedule.getConsultationDate())).append("</td>")
                        .append("<td>").append(getStringValue(schedule.getConsultationTime())).append("</td>")
                        .append("<td>").append(getStringValue(schedule.getLocation())).append("</td>")
                        .append("<td>").append(getStringValue(schedule.getLink())).append("</td>")
                        .append("<td>").append(getStringValue(schedule.getMode())).append("</td>")
                        .append("<td>").append(getStringValue(schedule.getStatusPublic())).append("</td>")
                        .append("<td>").append(getStringValue(schedule.getStatusConfirmed())).append("</td>");
            } else if (item instanceof ConversationDTO) {
                ConversationDTO conversation = (ConversationDTO) item;
                dataRows.append("<td>").append(getStringValue(conversation.getId())).append("</td>")
                        .append("<td>").append(getStringValue(conversation.getName())).append("</td>")
                        .append("<td>").append(getStringValue(conversation.getDepartment().getName())).append("</td>")
                        .append("<td>").append(getStringValue(conversation.getCreatedAt())).append("</td>")
                        .append("<td>").append(getStringValue(conversation.getIsGroup())).append("</td>");
            } else if (item instanceof ForwardQuestionDTO) {
                ForwardQuestionDTO question = (ForwardQuestionDTO) item;
                dataRows.append("<td>").append(getStringValue(question.getId())).append("</td>")
                        .append("<td>").append(getStringValue(question.getFromDepartment().getName())).append("</td>")
                        .append("<td>").append(getStringValue(question.getToDepartment().getName())).append("</td>")
                        .append("<td>").append(getStringValue(question.getConsultant().getName())).append("</td>");
            } else if (item instanceof MyQuestionDTO) {
                MyQuestionDTO question = (MyQuestionDTO) item;
                dataRows.append("<td>").append(getStringValue(question.getId())).append("</td>")
                        .append("<td>").append(getStringValue(question.getDepartment().getName())).append("</td>")
                        .append("<td>").append(getStringValue(question.getField().getName())).append("</td>")
                        .append("<td>").append(getStringValue(question.getAskerLastname() + " " + question.getAskerFirstname())).append("</td>")
                        .append("<td>").append(getStringValue(question.getTitle())).append("</td>")
                        .append("<td>").append(getStringValue(question.getContent())).append("</td>");
                if (question.getAnswerId() != null) {
                    dataRows.append("<td>").append(getStringValue(question.getAnswerUserLastname() + " " + question.getAnswerUserFirstname())).append("</td>")
                            .append("<td>").append(getStringValue(question.getAnswerTitle())).append("</td>")
                            .append("<td>").append(getStringValue(question.getAnswerContent())).append("</td>");
                } else {
                    dataRows.append("<td>Không có</td><td>N/A</td><td>N/A</td>");
                }
            } else if (item instanceof PostDTO) {
                PostDTO post = (PostDTO) item;
                dataRows.append("<td>").append(getStringValue(post.getId())).append("</td>")
                        .append("<td>").append(getStringValue(post.getTitle())).append("</td>")
                        .append("<td>").append(getStringValue(post.getContent())).append("</td>")
                        .append("<td>").append(getStringValue(post.getName())).append("</td>")
                        .append("<td>").append(getStringValue(post.getAvatarUrl())).append("</td>")
                        .append("<td>").append(getStringValue(post.getFileName())).append("</td>")
                        .append("<td>").append(getStringValue(post.getCreatedAt())).append("</td>")
                        .append("<td>").append(getStringValue(post.getViews())).append("</td>")
                        .append("<td>").append(getStringValue(post.isAnonymous())).append("</td>")
                        .append("<td>").append(getStringValue(post.isApproved())).append("</td>");
            } else if (item instanceof RatingDTO) {
                RatingDTO rating = (RatingDTO) item;
                dataRows.append("<td>").append(getStringValue(rating.getId())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getDepartment().getName())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getUser().getName())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getConsultant().getName())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getGeneralSatisfaction())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getGeneralComment())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getExpertiseKnowledge())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getExpertiseComment())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getAttitude())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getAttitudeComment())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getResponseSpeed())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getResponseSpeedComment())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getUnderstanding())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getUnderstandingComment())).append("</td>")
                        .append("<td>").append(getStringValue(rating.getSubmittedAt())).append("</td>");
            } else if (item instanceof ManageAccountDTO) {
                ManageAccountDTO account = (ManageAccountDTO) item;
                dataRows.append("<td>").append(getStringValue(account.getId())).append("</td>")
                        .append("<td>").append(getStringValue(account.getUsername())).append("</td>")
                        .append("<td>").append(getStringValue(account.getEmail())).append("</td>")
                        .append("<td>").append(getStringValue(account.getCreatedAt())).append("</td>")
                        .append("<td>").append(getStringValue(
                                account.getDepartment() != null ? account.getDepartment().getName() : null)).append("</td>")
                        .append("<td>").append(getStringValue(
                                account.getRole() != null ? account.getRole().getName() : null)).append("</td>")
                        .append("<td>").append(getStringValue(
                                account.getRoleConsultant() != null ? account.getRoleConsultant().getName() : null)).append("</td>")
                        .append("<td>").append(getStringValue(account.getIsOnline())).append("</td>");
            }
            else if (item instanceof ManageAddressDTO) {
                ManageAddressDTO address = (ManageAddressDTO) item;
                dataRows.append("<td>").append(getStringValue(address.getId())).append("</td>")
                        .append("<td>").append(getStringValue(address.getLine())).append("</td>")
                        .append("<td>").append(getStringValue(address.getProvinceCode())).append("</td>")
                        .append("<td>").append(getStringValue(address.getDistrictCode())).append("</td>")
                        .append("<td>").append(getStringValue(address.getWardCode())).append("</td>");
            } else if (item instanceof ManageDepartmentDTO) {
                ManageDepartmentDTO department = (ManageDepartmentDTO) item;
                dataRows.append("<td>").append(getStringValue(department.getId())).append("</td>")
                        .append("<td>").append(getStringValue(department.getName())).append("</td>")
                        .append("<td>").append(getStringValue(department.getCreatedAt())).append("</td>")
                        .append("<td>").append(getStringValue(department.getDescription())).append("</td>")
                        .append("<td>").append(getStringValue(department.getLogo())).append("</td>");
            } else if (item instanceof ManageDistrictDTO) {
                ManageDistrictDTO district = (ManageDistrictDTO) item;
                dataRows.append("<td>").append(getStringValue(district.getCode())).append("</td>")
                        .append("<td>").append(getStringValue(district.getName())).append("</td>")
                        .append("<td>").append(getStringValue(district.getNameEn())).append("</td>")
                        .append("<td>").append(getStringValue(district.getFullName())).append("</td>")
                        .append("<td>").append(getStringValue(district.getFullNameEn())).append("</td>")
                        .append("<td>").append(getStringValue(district.getCodeName())).append("</td>")
                        .append("<td>").append(getStringValue(district.getProvinceCode())).append("</td>");
            } else if (item instanceof ManageFieldDTO) {
                ManageFieldDTO field = (ManageFieldDTO) item;
                dataRows.append("<td>").append(getStringValue(field.getId())).append("</td>")
                        .append("<td>").append(getStringValue(field.getName())).append("</td>")
                        .append("<td>").append(getStringValue(field.getCreatedAt())).append("</td>")
                        .append("<td>").append(getStringValue(field.getDepartmentId())).append("</td>");
            } else if (item instanceof ManageProvinceDTO) {
                ManageProvinceDTO province = (ManageProvinceDTO) item;
                dataRows.append("<td>").append(getStringValue(province.getCode())).append("</td>")
                        .append("<td>").append(getStringValue(province.getName())).append("</td>")
                        .append("<td>").append(getStringValue(province.getNameEn())).append("</td>")
                        .append("<td>").append(getStringValue(province.getFullName())).append("</td>")
                        .append("<td>").append(getStringValue(province.getFullNameEn())).append("</td>")
                        .append("<td>").append(getStringValue(province.getCodeName())).append("</td>");
            } else if (item instanceof ManageRoleAskDTO) {
                ManageRoleAskDTO roleAsk = (ManageRoleAskDTO) item;
                dataRows.append("<td>").append(getStringValue(roleAsk.getId())).append("</td>")
                        .append("<td>").append(getStringValue(roleAsk.getName())).append("</td>")
                        .append("<td>").append(getStringValue(roleAsk.getRoleId())).append("</td>")
                        .append("<td>").append(getStringValue(roleAsk.getCreatedAt())).append("</td>");
            } else if (item instanceof ManageRoleConsultantDTO) {
                ManageRoleConsultantDTO roleConsultant = (ManageRoleConsultantDTO) item;
                dataRows.append("<td>").append(getStringValue(roleConsultant.getId())).append("</td>")
                        .append("<td>").append(getStringValue(roleConsultant.getName())).append("</td>")
                        .append("<td>").append(getStringValue(roleConsultant.getRoleId())).append("</td>")
                        .append("<td>").append(getStringValue(roleConsultant.getCreatedAt())).append("</td>");
            } else if (item instanceof RoleDTO) {
                RoleDTO role = (RoleDTO) item;
                dataRows.append("<td>").append(getStringValue(role.getId())).append("</td>")
                        .append("<td>").append(getStringValue(role.getName())).append("</td>");
            } else if (item instanceof ManageUserDTO) {
                ManageUserDTO user = (ManageUserDTO) item;
                dataRows.append("<td>").append(getStringValue(user.getId())).append("</td>")
                        .append("<td>").append(getStringValue(user.getLastName() + " " + user.getFirstName())).append("</td>")
                        .append("<td>").append(getStringValue(user.getStudentCode())).append("</td>")
                        .append("<td>").append(getStringValue(user.getGender())).append("</td>")
                        .append("<td>").append(getStringValue(user.getPhone())).append("</td>")
                        .append("<td>").append(getStringValue(user.getCreatedAt())).append("</td>")
                        .append("<td>").append(getStringValue(user.getAddress().getLine())).append("</td>")
                        .append("<td>").append(getStringValue(user.getAddress().getProvinceFullName())).append("</td>")
                        .append("<td>").append(getStringValue(user.getAddress().getDistrictFullName())).append("</td>")
                        .append("<td>").append(getStringValue(user.getAddress().getWardFullName())).append("</td>");
            } else if (item instanceof ManageWardDTO) {
                ManageWardDTO ward = (ManageWardDTO) item;
                dataRows.append("<td>").append(getStringValue(ward.getCode())).append("</td>")
                        .append("<td>").append(getStringValue(ward.getName())).append("</td>")
                        .append("<td>").append(getStringValue(ward.getNameEn())).append("</td>")
                        .append("<td>").append(getStringValue(ward.getFullName())).append("</td>")
                        .append("<td>").append(getStringValue(ward.getFullNameEn())).append("</td>")
                        .append("<td>").append(getStringValue(ward.getCodeName())).append("</td>")
                        .append("<td>").append(getStringValue(ward.getDistrictCode())).append("</td>");
            }

            dataRows.append("</tr>");
        }

        return dataRows.toString();
    }
}
