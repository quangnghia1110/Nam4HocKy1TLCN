package studentConsulting.service.implement.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.*;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.CommonQuestionDTO;
import studentConsulting.model.payload.mapper.actor.CommonQuestionMapper;
import studentConsulting.model.payload.request.CommonQuestionRequest;
import studentConsulting.repository.actor.AnswerRepository;
import studentConsulting.repository.actor.CommonQuestionRepository;
import studentConsulting.repository.actor.QuestionRepository;
import studentConsulting.repository.admin.DepartmentRepository;
import studentConsulting.repository.admin.FieldRepository;
import studentConsulting.repository.admin.RoleAskRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.implement.common.FileStorageServiceImpl;
import studentConsulting.service.interfaces.actor.ICommonQuestionService;
import studentConsulting.specification.actor.CommonQuestionSpecification;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@Service
public class CommonQuestionServiceImpl implements ICommonQuestionService {

    @Autowired
    private CommonQuestionRepository commonQuestionRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private FieldRepository fieldRepository;

    @Autowired
    private RoleAskRepository roleAskRepository;

    @Autowired
    private FileStorageServiceImpl fileStorageService;

    @Autowired
    private CommonQuestionMapper commonQuestionMapper;

    public void handleFile(CommonQuestionEntity commonQuestion, MultipartFile file) {
        if (file != null && !file.isEmpty()) {
            if (commonQuestion.getFileName() != null) {
                fileStorageService.deleteFile(commonQuestion.getFileName());
            }
            String fileName = fileStorageService.saveFile(file);
            commonQuestion.setFileName(fileName);
        } else {
            if (commonQuestion.getFileName() != null) {
                fileStorageService.deleteFile(commonQuestion.getFileName());
                commonQuestion.setFileName(null);
            }
        }
    }

    @Override
    @Transactional
    public CommonQuestionDTO convertToCommonQuestion(Integer questionId, Principal principal) {
        String email = principal.getName();
        UserInformationEntity createdByUser = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        QuestionEntity question = questionRepository.findById(questionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

        boolean isAdmin = createdByUser.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        if (!isAdmin && !question.getDepartment().getId().equals(createdByUser.getAccount().getDepartment().getId())) {
            throw new ErrorException("Câu hỏi không thuộc phòng ban của bạn");
        }

        return createCommonQuestionFromQuestionEntity(question, createdByUser);
    }

    private CommonQuestionDTO createCommonQuestionFromQuestionEntity(QuestionEntity question, UserInformationEntity createdByUser) {
        CommonQuestionEntity commonQuestion = new CommonQuestionEntity();
        commonQuestion.setTitle(question.getTitle());
        commonQuestion.setContent(question.getContent());
        commonQuestion.setFileName(question.getFileName());
        commonQuestion.setCreatedAt(question.getCreatedAt());
        commonQuestion.setViews(question.getViews());
        commonQuestion.setDepartment(question.getDepartment());
        commonQuestion.setField(question.getField());
        commonQuestion.setRoleAsk(question.getRoleAsk());
        commonQuestion.setUser(question.getUser());
        commonQuestion.setCreatedBy(createdByUser);
        commonQuestion.setStatus(false);

        Optional<AnswerEntity> firstAnswer = answerRepository.findFirstAnswerByQuestionId(question.getId());
        if (firstAnswer.isPresent()) {
            AnswerEntity answer = firstAnswer.get();
            commonQuestion.setAnswerContent(answer.getContent());
            commonQuestion.setAnswerTitle(answer.getTitle());
            commonQuestion.setAnswerCreatedAt(answer.getCreatedAt());
            commonQuestion.setAnswerUserEmail(createdByUser.getAccount().getEmail());
            commonQuestion.setAnswerUserLastname(answer.getUser().getLastName());
            commonQuestion.setAnswerUserFirstname(answer.getUser().getFirstName());
            commonQuestion.setAskerLastname(answer.getQuestion().getUser().getLastName());
            commonQuestion.setAskerFirstname(answer.getQuestion().getUser().getFirstName());
        }

        CommonQuestionEntity savedCommonQuestion = commonQuestionRepository.save(commonQuestion);

        return commonQuestionMapper.mapToDTO(savedCommonQuestion);
    }

    @Override
    public CommonQuestionDTO createCommonQuestion(CommonQuestionRequest request, MultipartFile file, Principal principal) {
        String email = principal.getName();
        UserInformationEntity createdByUser = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        DepartmentEntity department = departmentRepository.findById(request.getDepartmentId())
                .orElseThrow(() -> new ErrorException("Phòng ban không tồn tại"));

        FieldEntity field = fieldRepository.findById(request.getFieldId())
                .orElseThrow(() -> new ErrorException("Lĩnh vực không tồn tại"));

        RoleAskEntity roleAsk = roleAskRepository.findById(request.getRoleAskId())
                .orElseThrow(() -> new ErrorException("Vai trò hỏi không tồn tại"));

        CommonQuestionEntity commonQuestion = new CommonQuestionEntity();
        commonQuestion.setTitle(request.getTitle());
        commonQuestion.setContent(request.getContent());
        handleFile(commonQuestion, file);
        commonQuestion.setCreatedAt(request.getCreatedAt());
        commonQuestion.setViews(0);
        commonQuestion.setDepartment(department);
        commonQuestion.setField(field);
        commonQuestion.setRoleAsk(roleAsk);
        commonQuestion.setCreatedBy(createdByUser);
        commonQuestion.setStatus(false);

        commonQuestion.setAnswerContent(request.getAnswerContent());
        commonQuestion.setAnswerTitle(request.getAnswerTitle());
        commonQuestion.setAnswerCreatedAt(request.getAnswerCreatedAt());
        commonQuestion.setAnswerUserLastname(request.getAnswerUserLastname());
        commonQuestion.setAnswerUserFirstname(request.getAnswerUserFirstname());
        commonQuestion.setAskerLastname(request.getAskerLastname());
        commonQuestion.setAskerFirstname(request.getAskerFirstname());
        commonQuestion.setAnswerUserEmail(createdByUser.getAccount().getEmail());

        CommonQuestionEntity savedCommonQuestion = commonQuestionRepository.save(commonQuestion);

        return commonQuestionMapper.mapToDTO(savedCommonQuestion);
    }

    @Override
    @Transactional
    public CommonQuestionDTO updateCommonQuestion(Integer commonQuestionId, MultipartFile file,CommonQuestionRequest request, Principal principal) {
        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        CommonQuestionEntity existingCommonQuestion = commonQuestionRepository.findById(commonQuestionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi chung không tồn tại"));

        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isTruongBanTuvan = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);

        if (!isAdmin && !isTruongBanTuvan) {
            throw new ErrorException("Bạn không có quyền chỉnh sửa câu hỏi này.");
        }
        if (isAdmin) {
            existingCommonQuestion.setStatus(request.getStatus());
        }

        if (isAdmin) {
            DepartmentEntity department = departmentRepository.findById(request.getDepartmentId())
                    .orElseThrow(() -> new ErrorException("Phòng ban không tồn tại"));
            existingCommonQuestion.setDepartment(department);
        }

        existingCommonQuestion.setTitle(request.getTitle());
        existingCommonQuestion.setContent(request.getContent());
        handleFile(existingCommonQuestion, file);
        existingCommonQuestion.setCreatedAt(request.getCreatedAt());

        existingCommonQuestion.setAnswerTitle(request.getAnswerTitle());
        existingCommonQuestion.setAnswerContent(request.getAnswerContent());
        existingCommonQuestion.setAnswerCreatedAt(request.getAnswerCreatedAt());
        existingCommonQuestion.setAnswerUserLastname(request.getAnswerUserLastname());
        existingCommonQuestion.setAnswerUserFirstname(request.getAnswerUserFirstname());

        existingCommonQuestion.setAskerLastname(request.getAskerLastname());
        existingCommonQuestion.setAskerFirstname(request.getAskerFirstname());
        existingCommonQuestion.setAnswerUserEmail(request.getAnswerEmail());

        CommonQuestionEntity updatedCommonQuestion = commonQuestionRepository.save(existingCommonQuestion);

        return commonQuestionMapper.mapToDTO(updatedCommonQuestion);
    }

    @Override
    @Transactional
    public void deleteCommonQuestion(Integer id, UserInformationEntity user) {
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        if (isAdmin) {
            commonQuestionRepository.deleteById(id);
        } else {
            commonQuestionRepository.findByIdAndDepartmentId(id, user.getAccount().getDepartment().getId())
                    .ifPresentOrElse(commonQuestionRepository::delete,
                            () -> { throw new ErrorException("Không tìm thấy câu hỏi tổng hợp hoặc bạn không có quyền xóa"); });
        }
    }


    @Override
    public CommonQuestionDTO getCommonQuestionById(Integer questionId, UserInformationEntity user) {
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        if (isAdmin) {
            return commonQuestionRepository.findById(questionId)
                    .map(commonQuestionMapper::mapToDTO)
                    .orElseThrow(() -> new ErrorException("Không tìm thấy câu hỏi"));
        } else {
            return commonQuestionRepository.findByIdAndDepartmentId(questionId, user.getAccount().getDepartment().getId())
                    .map(commonQuestionMapper::mapToDTO)
                    .orElseThrow(() -> new ErrorException("Không tìm thấy câu hỏi"));
        }
    }


    @Override
    public Page<CommonQuestionDTO> getCommonQuestionByRole(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<CommonQuestionEntity> spec = Specification.where(null);

        if (departmentId != null) {
            spec = spec.and(CommonQuestionSpecification.isCreatedByAdvisor(departmentId));
        }

        if (title != null && !title.isEmpty()) {
            spec = spec.and(CommonQuestionSpecification.hasTitle(title));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasDateBefore(endDate));
        }

        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findAll(spec, pageable);
        return commonQuestions.map(commonQuestionMapper::mapToDTO);
    }
}
