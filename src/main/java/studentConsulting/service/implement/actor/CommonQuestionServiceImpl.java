package studentConsulting.service.implement.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.*;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.CommonQuestionDTO;
import studentConsulting.model.payload.dto.actor.QuestionDTO;
import studentConsulting.model.payload.mapper.actor.CommonQuestionMapper;
import studentConsulting.model.payload.request.CommonQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
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
            if (commonQuestion.getFile() != null) {
                fileStorageService.deleteFile(commonQuestion.getFile());
            }
            String fileName = fileStorageService.saveFile(file);
            commonQuestion.setFile(fileName);
        } else {
            if (commonQuestion.getFile() != null) {
                fileStorageService.deleteFile(commonQuestion.getFile());
                commonQuestion.setFile(null);
            }
        }
    }

    public void handleFileAnswer(CommonQuestionEntity commonQuestion, MultipartFile file) {
        if (file != null && !file.isEmpty()) {
            if (commonQuestion.getFileAnswer() != null) {
                fileStorageService.deleteFile(commonQuestion.getFileAnswer());
            }
            String fileName = fileStorageService.saveFile(file);
            commonQuestion.setFileAnswer(fileName);
        } else {
            if (commonQuestion.getFileAnswer() != null) {
                fileStorageService.deleteFile(commonQuestion.getFileAnswer());
                commonQuestion.setFileAnswer(null);
            }
        }
    }
    @Override
    @Transactional
    public CommonQuestionDTO convertToCommonQuestion(Integer questionId, MultipartFile file, MultipartFile fileAnswer, Principal principal) {
        String email = principal.getName();
        UserInformationEntity createdByUser = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        QuestionEntity question = questionRepository.findById(questionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

        boolean isAdmin = createdByUser.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        if (!isAdmin && !question.getDepartment().getId().equals(createdByUser.getAccount().getDepartment().getId())) {
            throw new ErrorException("Câu hỏi không thuộc phòng ban của bạn");
        }

        return createCommonQuestionFromQuestionEntity(question, file, fileAnswer,createdByUser);
    }

    private CommonQuestionDTO createCommonQuestionFromQuestionEntity(QuestionEntity question, MultipartFile file, MultipartFile fileAnswer, UserInformationEntity createdByUser) {
        CommonQuestionEntity commonQuestion = new CommonQuestionEntity();
        commonQuestion.setDepartment(question.getDepartment());
        commonQuestion.setTitle(question.getTitle());
        commonQuestion.setContent(question.getContent());
        commonQuestion.setCreatedAt(LocalDate.now());
        commonQuestion.setCreatedBy(createdByUser);
        commonQuestion.setStatus(false);
        handleFile(commonQuestion, file);

        Optional<AnswerEntity> firstAnswer = answerRepository.findFirstAnswerByQuestionId(question.getId());
        if (firstAnswer.isPresent()) {
            AnswerEntity answer = firstAnswer.get();
            commonQuestion.setAnswerContent(answer.getContent());
            commonQuestion.setAnswerTitle(answer.getTitle());
            handleFileAnswer(commonQuestion, fileAnswer);
        }

        CommonQuestionEntity savedCommonQuestion = commonQuestionRepository.save(commonQuestion);

        return commonQuestionMapper.mapToDTO(savedCommonQuestion);
    }

    @Override
    public CommonQuestionDTO createCommonQuestion(CommonQuestionRequest request, MultipartFile file, MultipartFile fileAnswer, Principal principal) {
        String email = principal.getName();
        UserInformationEntity createdByUser = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        CommonQuestionEntity commonQuestion = new CommonQuestionEntity();
        commonQuestion.setTitle(request.getTitle());
        commonQuestion.setContent(request.getContent());
        commonQuestion.setCreatedAt(LocalDate.now());
        commonQuestion.setCreatedBy(createdByUser);
        commonQuestion.setStatus(false);
        if (request.getDepartmentId() != null) {

        DepartmentEntity department = departmentRepository.findById(request.getDepartmentId())
                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban"));
        commonQuestion.setDepartment(department);
        }
        handleFile(commonQuestion, file);

        commonQuestion.setAnswerContent(request.getContent());
        commonQuestion.setAnswerTitle(request.getTitle());
        handleFileAnswer(commonQuestion, fileAnswer);

        CommonQuestionEntity savedCommonQuestion = commonQuestionRepository.save(commonQuestion);

        return commonQuestionMapper.mapToDTO(savedCommonQuestion);
    }

    @Override
    public DataResponse<CommonQuestionDTO> updateCommonQuestion(Integer commonQuestionId, MultipartFile file, MultipartFile fileAnswer,CommonQuestionRequest request) {
        CommonQuestionEntity existingCommonQuestion = commonQuestionRepository.findById(commonQuestionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi chung không tồn tại"));

        existingCommonQuestion.setTitle(request.getTitle());
        existingCommonQuestion.setContent(request.getContent());
        existingCommonQuestion.setAnswerTitle(request.getAnswerTitle());
        existingCommonQuestion.setAnswerContent(request.getAnswerContent());
        existingCommonQuestion.setStatus(request.getStatus());

        handleFile(existingCommonQuestion, file);

        handleFileAnswer(existingCommonQuestion, fileAnswer);

        CommonQuestionEntity updatedCommonQuestion = commonQuestionRepository.save(existingCommonQuestion);

        CommonQuestionDTO updatedCommonQuestionDTO = commonQuestionMapper.mapToDTO(updatedCommonQuestion);

        return DataResponse.<CommonQuestionDTO>builder()
                .status("success")
                .message("Câu hỏi chung đã được cập nhật thành công.")
                .data(updatedCommonQuestionDTO)
                .build();
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
