package studentConsulting.service.implement.actor;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.department_field.FieldEntity;
import studentConsulting.model.entity.question_answer.DeletionLogEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.DeletionLogDTO;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.model.payload.dto.question_answer.QuestionDTO;
import studentConsulting.model.payload.dto.user.RoleAskDTO;
import studentConsulting.model.payload.mapper.actor.QuestionMapper;
import studentConsulting.model.payload.request.question_answer.CreateFollowUpQuestionRequest;
import studentConsulting.model.payload.request.question_answer.CreateQuestionRequest;
import studentConsulting.model.payload.request.question_answer.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.department_field.FieldRepository;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.DeletionLogRepository;
import studentConsulting.repository.question_answer.ForwardQuestionRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.user.RoleAskRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.implement.common.FileStorageServiceImpl;
import studentConsulting.service.interfaces.actor.IQuestionService;
import studentConsulting.specification.question_answer.QuestionSpecification;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class QuestionServiceImpl implements IQuestionService {

    @Autowired
    private Cloudinary cloudinary;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private FieldRepository fieldRepository;

    @Autowired
    private RoleAskRepository roleAskRepository;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private DeletionLogRepository deletionLogRepository;

    @Autowired
    private ForwardQuestionRepository forwardQuestionRepository;

    @Autowired
    private FileStorageServiceImpl fileStorageService;

    @Autowired
    private QuestionMapper questionMapper;

    @Override
    public List<RoleAskDTO> getAllRoleAsk() {
        return roleAskRepository.findAll().stream().map(roleAsk -> new RoleAskDTO(roleAsk.getId(), roleAsk.getName()))
                .collect(Collectors.toList());
    }

    @Override
    public DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest, Integer userId) {
        String fileName = null;
        if (questionRequest.getFile() != null && !questionRequest.getFile().isEmpty()) {
            fileName = fileStorageService.saveFile(questionRequest.getFile());
        }

        QuestionDTO questionDTO = questionMapper.mapRequestToDTO(questionRequest, fileName);
        QuestionEntity question = questionMapper.mapDTOToEntity(questionDTO, userId);
        question.setStatusApproval(false);
        question.setViews(0);

        QuestionEntity savedQuestion = questionRepository.save(question);
        questionRepository.save(savedQuestion);

        QuestionDTO savedQuestionDTO = questionMapper.mapEntityToDTO(savedQuestion);

        return DataResponse.<QuestionDTO>builder().status("success").message("Câu hỏi đã được tạo")
                .data(savedQuestionDTO).build();
    }

    @Override
    public DataResponse<QuestionDTO> updateQuestion(Integer questionId, UpdateQuestionRequest request) {
        QuestionEntity existingQuestion = questionRepository.findById(questionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

        if (Boolean.TRUE.equals(existingQuestion.getStatusApproval())) {
            throw new ErrorException("Câu hỏi đã được duyệt, không thể chỉnh sửa.");
        }

        existingQuestion.setTitle(request.getTitle());
        existingQuestion.setContent(request.getContent());
        existingQuestion.setStatusPublic(request.getStatusPublic());

        existingQuestion.setDepartment(
                departmentRepository.findById(request.getDepartmentId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Phòng ban không tồn tại với id: " + request.getDepartmentId()))
        );

        existingQuestion.setField(
                fieldRepository.findById(request.getFieldId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Lĩnh vực không tồn tại với id: " + request.getFieldId()))
        );

        existingQuestion.setRoleAsk(
                roleAskRepository.findById(request.getRoleAskId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Vai trò không tồn tại với id: " + request.getRoleAskId()))
        );

        UserInformationEntity user = existingQuestion.getUser();
        user.setFirstName(request.getFirstName());
        user.setLastName(request.getLastName());
        existingQuestion.setUser(user);

        if (request.getFile() != null && !request.getFile().isEmpty()) {
            String fileName = fileStorageService.saveFile(request.getFile());
            existingQuestion.setFileName(fileName);
        }
        existingQuestion.setViews(existingQuestion.getViews());
        existingQuestion.setStatusApproval(false);

        QuestionEntity updatedQuestion = questionRepository.save(existingQuestion);
        QuestionDTO updatedQuestionDTO = questionMapper.mapEntityToDTO(updatedQuestion);

        return DataResponse.<QuestionDTO>builder().status("success").message("Câu hỏi đã được cập nhật thành công.")
                .data(updatedQuestionDTO).build();
    }

    @Override
    @Transactional
    public DataResponse<Void> deleteQuestion(Integer questionId, String email) {
        QuestionEntity existingQuestion = questionRepository.findById(questionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

        Optional<DeletionLogEntity> existingLog = deletionLogRepository.findByQuestionId(questionId);
        if (existingLog.isPresent()) {
            throw new ErrorException("Câu hỏi đã bị xóa trước đó.");
        }

        if (Boolean.TRUE.equals(existingQuestion.getStatusApproval())) {
            throw new ErrorException("Câu hỏi đã được duyệt, không thể xóa.");
        }

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        UserInformationEntity user = userOpt.orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

        DeletionLogEntity deletionLog = DeletionLogEntity.builder().question(existingQuestion)
                .reason("Xóa theo yêu cầu của bản thân").deletedBy(user.getAccount().getUsername())
                .deletedAt(LocalDate.now()).build();

        deletionLogRepository.save(deletionLog);
        questionRepository.softDeleteQuestion(questionId);

        return DataResponse.<Void>builder().status("success").message("Câu hỏi đã được xóa thành công.").build();
    }

    @Override
    public DataResponse<QuestionDTO> askFollowUpQuestion(Integer parentQuestionId, String title, String content,
                                                         MultipartFile file, Integer userId) {
        QuestionEntity parentQuestion = questionRepository.findById(parentQuestionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi cha không tồn tại"));

        String fileName = null;
        if (file != null && !file.isEmpty()) {
            fileName = fileStorageService.saveFile(file);
        }

        CreateFollowUpQuestionRequest followUpRequest = CreateFollowUpQuestionRequest.builder()
                .parentQuestionId(parentQuestionId).departmentId(parentQuestion.getDepartment().getId())
                .fieldId(parentQuestion.getField().getId()).roleAskId(parentQuestion.getRoleAsk().getId())
                .firstName(parentQuestion.getUser().getFirstName()).lastName(parentQuestion.getUser().getLastName())
                .title(title).content(content).statusPublic(parentQuestion.getStatusPublic()).file(file)
                .statusApproval(false).build();

        QuestionDTO followUpQuestionDTO = questionMapper.mapRequestToDTO(followUpRequest, fileName);
        QuestionEntity followUpQuestion = questionMapper.mapDTOToEntity(followUpQuestionDTO, userId);

        followUpQuestion.setUser(
                userRepository.findById(userId).orElseThrow(() -> new ErrorException("Người dùng không tồn tại")));

        followUpQuestion.setParentQuestion(parentQuestion);
        followUpQuestion.setStatusApproval(false);
        followUpQuestion.setViews(parentQuestion.getViews());

        QuestionEntity savedFollowUpQuestion = questionRepository.save(followUpQuestion);
        QuestionDTO savedFollowUpQuestionDTO = questionMapper.mapEntityToDTO(savedFollowUpQuestion);

        return DataResponse.<QuestionDTO>builder().status("success").message("Câu hỏi tiếp theo đã được tạo")
                .data(savedFollowUpQuestionDTO).build();
    }

    @Override
    public Page<MyQuestionDTO> getAllQuestionsWithFilters(Integer departmentId, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<QuestionEntity> spec = Specification.where(QuestionSpecification.isPublicAndAnswered());

        if (departmentId != null) {
            spec = spec.and(QuestionSpecification.hasConsultantsInDepartment(departmentId));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(QuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(QuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(QuestionSpecification.hasDateBefore(endDate));
        }

        Page<QuestionEntity> questions = questionRepository.findAll(spec, pageable);

        return questions.map(question -> questionMapper.mapToMyQuestionDTO(question, new HashSet<>()));
    }

    @Override
    public Page<MyQuestionDTO> getQuestionAnswerByRole(UserInformationEntity user, String title, String status, Integer departmentId, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<QuestionEntity> spec = Specification.where(null);

        String userRole = user.getAccount().getRole().getName();
        Integer depId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;
        Integer consultantId = user.getId();
        Integer userId = user.getId();

        if (userRole.equals(SecurityConstants.Role.USER)) {
            spec = spec.and(QuestionSpecification.hasUserQuestion(userId));
        } else if (userRole.equals(SecurityConstants.Role.TRUONGBANTUVAN)) {
            if (depId != null) {
                spec = spec.and(QuestionSpecification.hasDepartments(depId));
            }
        } else if (userRole.equals(SecurityConstants.Role.TUVANVIEN)) {
            if (depId != null) {
                System.out.println(depId);
                spec = spec.and(QuestionSpecification.hasDepartments(depId));
            }

            System.out.println(consultantId);
            spec = spec.and(Specification.where(
                    QuestionSpecification.hasStatus(QuestionFilterStatus.NOT_ANSWERED)
                            .or(QuestionSpecification.hasConsultantAnswer(consultantId))
            ));
        } else if (userRole.equals(SecurityConstants.Role.ADMIN)) {

        } else {
            throw new ErrorException("Bạn không có quyền thực hiện hành động này");
        }

        if (title != null && !title.isEmpty()) {
            spec = spec.and(QuestionSpecification.hasTitle(title));
        }

        if (departmentId != null) {
            spec = spec.and(QuestionSpecification.hasConsultantsInDepartment(departmentId));
        }

        if (status != null && !status.isEmpty()) {
            QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);
            spec = spec.and(QuestionSpecification.hasStatus(filterStatus));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(QuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(QuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(QuestionSpecification.hasDateBefore(endDate));
        }

        Page<QuestionEntity> questionEntities = questionRepository.findAll(spec, pageable);

        return questionEntities.map(questionMapper::mapToMyQuestionDTO);
    }

    @Override
    @Transactional
    public DataResponse<String> deleteQuestion(Integer questionId, String reason, String email) {
        QuestionEntity question = questionRepository.findById(questionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

        Optional<DeletionLogEntity> existingLog = deletionLogRepository.findByQuestionId(questionId);
        if (existingLog.isPresent()) {
            throw new ErrorException("Câu hỏi đã bị xóa trước đó.");
        }

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        UserInformationEntity user = userOpt.orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

        if (!SecurityConstants.Role.TUVANVIEN.equals(user.getAccount().getRole().getName())
                && !SecurityConstants.Role.TRUONGBANTUVAN.equals(user.getAccount().getRole().getName())
                && !SecurityConstants.Role.ADMIN.equals(user.getAccount().getRole().getName())) {
            throw new ErrorException("Bạn không có quyền xóa câu hỏi này.");
        }


        DeletionLogEntity deletionLog = DeletionLogEntity.builder().question(question).reason(reason)
                .deletedBy(user.getAccount().getEmail()).deletedAt(LocalDate.now()).build();

        deletionLogRepository.save(deletionLog);
        questionRepository.softDeleteQuestion(questionId);

        return DataResponse.<String>builder().status("success").message("Câu hỏi đã được xóa thành công.").build();
    }

    @Override
    public MyQuestionDTO getQuestionDetail(Integer consultantId, Integer questionId) {
        QuestionEntity question = questionRepository.findById(questionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

        DepartmentEntity consultantDepartment = userRepository.findConsultantDepartmentByConsultantId(consultantId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban của tư vấn viên"));

        DepartmentEntity questionDepartment = question.getDepartment();

        if (!consultantDepartment.equals(questionDepartment)) {
            throw new ErrorException("Bạn không có quyền truy cập vào câu hỏi này.");
        }

        return questionMapper.mapToMyQuestionDTO(question);
    }


    @Override
    public Page<DeletionLogDTO> getDeletionLogs(UserInformationEntity user, Pageable pageable) {
        Specification<DeletionLogEntity> spec = Specification.where(null);
        String userRole = user.getAccount().getRole().getName();
        Integer departmentId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        switch (userRole) {
            case SecurityConstants.Role.ADMIN:
                break;

            case SecurityConstants.Role.TRUONGBANTUVAN:
                if (departmentId != null) {
                    spec = spec.and(QuestionSpecification.belongsToDepartment(departmentId));
                } else {
                    throw new ErrorException("Trưởng ban không thuộc phòng ban nào.");
                }
                break;

            case SecurityConstants.Role.TUVANVIEN:
                String deletedBy = user.getAccount().getEmail();
                spec = spec.and(QuestionSpecification.deletedByEmail(deletedBy));
                break;

            default:
                throw new ErrorException("Bạn không có quyền thực hiện hành động này");
        }

        Page<DeletionLogEntity> deletionLogs = deletionLogRepository.findAll(spec, pageable);
        return deletionLogs.map(questionMapper::mapToDeletionLogDTO);
    }


    @Override
    public DeletionLogDTO getDeletionLogDetail(UserInformationEntity user, Integer questionId) {
        Specification<DeletionLogEntity> spec;
        String userRole = user.getAccount().getRole().getName();
        Integer departmentId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        switch (userRole) {
            case SecurityConstants.Role.ADMIN:
                spec = Specification.where(QuestionSpecification.hasQuestionId(questionId));
                break;

            case SecurityConstants.Role.TRUONGBANTUVAN:
                if (departmentId != null) {
                    spec = Specification.where(QuestionSpecification.hasQuestionId(questionId))
                            .and(QuestionSpecification.belongsToDepartment(departmentId));
                } else {
                    throw new ErrorException("Trưởng ban không thuộc phòng ban nào.");
                }
                break;

            case SecurityConstants.Role.TUVANVIEN:
                String deletedBy = user.getAccount().getEmail();
                spec = Specification.where(QuestionSpecification.hasQuestionId(questionId))
                        .and(QuestionSpecification.deletedByEmail(deletedBy));
                break;

            default:
                throw new ErrorException("Bạn không có quyền thực hiện hành động này");
        }


        return deletionLogRepository.findOne(spec)
                .map(questionMapper::mapToDeletionLogDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy log xóa cho questionId đã cung cấp"));
    }


    @Override
    public void importQuestions(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)  // Skip header row
                .collect(Collectors.toList());

        List<MyQuestionDTO> questions = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String title = row.get(1);
                        String content = row.get(2);
                        LocalDate createdAt = LocalDate.parse(row.get(3));
                        String status = row.get(4);
                        Integer departmentId = Integer.parseInt(row.get(5));
                        Integer fieldId = Integer.parseInt(row.get(6));
                        String askerFirstname = row.get(7);
                        String askerLastname = row.get(8);
                        Integer views = Integer.parseInt(row.get(9));

                        return MyQuestionDTO.builder()
                                .id(id)
                                .title(title)
                                .content(content)
                                .createdAt(createdAt)
                                .department(new MyQuestionDTO.DepartmentDTO(departmentId, null))
                                .field(new MyQuestionDTO.FieldDTO(fieldId, null))
                                .askerFirstname(askerFirstname)
                                .askerLastname(askerLastname)
                                .views(views)
                                .build();
                    } catch (Exception e) {
                        throw new ErrorException("Lỗi khi parse dữ liệu câu hỏi: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        questions.forEach(question -> {
            try {
                QuestionEntity entity = new QuestionEntity();
                entity.setId(question.getId());
                entity.setTitle(question.getTitle());
                entity.setContent(question.getContent());
                entity.setCreatedAt(question.getCreatedAt());
                entity.setViews(question.getViews());

                DepartmentEntity department = departmentRepository.findById(question.getDepartment().getId())
                        .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với ID: " + question.getDepartment().getId()));
                FieldEntity field = fieldRepository.findById(question.getField().getId())
                        .orElseThrow(() -> new ErrorException("Không tìm thấy lĩnh vực với ID: " + question.getField().getId()));

                entity.setDepartment(department);
                entity.setField(field);

                questionRepository.save(entity);
            } catch (Exception e) {
                throw new ErrorException("Lỗi khi lưu câu hỏi vào database: " + e.getMessage());
            }
        });
    }

    //check lai
    @Override
    public Page<MyQuestionDTO> getDepartmentConsultantsQuestionsFilters(Integer departmentId, String title, String status, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<QuestionEntity> spec = Specification.where(QuestionSpecification.hasConsultantsInDepartment(departmentId));

        if (title != null && !title.isEmpty()) {
            spec = spec.and(QuestionSpecification.hasTitle(title));
        }

        if (status != null && !status.isEmpty()) {
            QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);
            spec = spec.and(QuestionSpecification.hasStatus(filterStatus));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(QuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(QuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(QuestionSpecification.hasDateBefore(endDate));
        }

        Page<QuestionEntity> questionEntities = questionRepository.findAll(spec, pageable);
        return questionEntities.map(questionMapper::mapToMyQuestionDTO);
    }
}