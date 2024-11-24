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
import studentConsulting.model.entity.DeletionLogEntity;
import studentConsulting.model.entity.DepartmentEntity;
import studentConsulting.model.entity.QuestionEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.DeletionLogDTO;
import studentConsulting.model.payload.dto.actor.MyQuestionDTO;
import studentConsulting.model.payload.dto.actor.QuestionDTO;
import studentConsulting.model.payload.mapper.actor.QuestionMapper;
import studentConsulting.model.payload.request.CreateFollowUpQuestionRequest;
import studentConsulting.model.payload.request.CreateQuestionRequest;
import studentConsulting.model.payload.request.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.actor.AnswerRepository;
import studentConsulting.repository.actor.DeletionLogRepository;
import studentConsulting.repository.actor.ForwardQuestionRepository;
import studentConsulting.repository.actor.QuestionRepository;
import studentConsulting.repository.admin.DepartmentRepository;
import studentConsulting.repository.admin.FieldRepository;
import studentConsulting.repository.admin.RoleAskRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.implement.common.FileStorageServiceImpl;
import studentConsulting.service.interfaces.actor.IQuestionService;
import studentConsulting.specification.actor.QuestionSpecification;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.Optional;

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

    public void handleFileForQuestion(QuestionEntity question, MultipartFile file) {
        if (file != null && !file.isEmpty()) {
            if (question.getFileName() != null) {
                fileStorageService.deleteFile(question.getFileName());
            }
            String fileName = fileStorageService.saveFile(file);
            question.setFileName(fileName);
        } else {
            if (question.getFileName() != null) {
                fileStorageService.deleteFile(question.getFileName());
                question.setFileName(null);
            }
        }
    }

    @Override
    public DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest, Integer userId) {
        String fileName = null;
        if (questionRequest.getFile() != null && !questionRequest.getFile().isEmpty()) {
            fileName = fileStorageService.saveFile(questionRequest.getFile());
        }

        QuestionDTO questionDTO = questionMapper.mapRequestToDTO(questionRequest, fileName);
        QuestionEntity question = questionMapper.mapDTOToEntity(
                questionDTO,
                userId,
                userRepository,
                departmentRepository,
                fieldRepository,
                roleAskRepository
        );
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

        handleFileForQuestion(existingQuestion, request.getFile());

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

        if (existingQuestion.getFileName() != null) {
            fileStorageService.deleteFile(existingQuestion.getFileName());
        }

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
        QuestionEntity followUpQuestion = questionMapper.mapDTOToEntity(
                followUpQuestionDTO,
                userId,
                userRepository,
                departmentRepository,
                fieldRepository,
                roleAskRepository
        );

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
    public Page<MyQuestionDTO> getQuestionAnswerByRole(Boolean statusApproval, UserInformationEntity user, String title, String status, Integer departmentId, LocalDate startDate, LocalDate endDate, Pageable pageable) {
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
        } else if (userRole.equals(SecurityConstants.Role.ADMIN)) {

        } else {
            throw new ErrorException("Bạn không có quyền thực hiện hành động này");
        }

        if (statusApproval != null) {
            if (statusApproval) {
                spec = spec.and(QuestionSpecification.hasApprovedAnswer());
            } else {
                spec = spec.and(QuestionSpecification.hasNoAnswerOrUnApprovedAnswer());
            }
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

        return questionEntities.map(question -> questionMapper.mapToMyQuestionDTO(question, answerRepository));
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

        return questionMapper.mapToMyQuestionDTO(question, answerRepository);
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
                .orElseThrow(null);
    }

}