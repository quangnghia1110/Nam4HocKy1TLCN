package studentConsulting.controller;

import java.security.Principal;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;
import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IAnswerService;

@RestController
@RequestMapping("/api/v1/answer")
public class AnswerController {

    @Autowired
    private IAnswerService answerService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @PostMapping(value = "/create", consumes = { "multipart/form-data" })
    public ResponseEntity<DataResponse<AnswerDTO>> createAnswer(
            @RequestParam("questionId") Integer questionId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestPart("file") MultipartFile file,
            @RequestParam("statusApproval") Boolean statusApproval,
            Principal principal) {

        // Lấy username của người dùng hiện tại
        String username = principal.getName();

        // Lấy thông tin người dùng dựa trên username
        Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
        if (userOpt.isEmpty()) {
            return ResponseEntity.badRequest().body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("User not found.")
                    .build()
            );
        }

        UserInformationEntity user = userOpt.get();
        RoleConsultantEntity roleConsultant = user.getAccount().getRoleConsultant(); 

        // Tạo request DTO
        CreateAnswerRequest answerRequest = CreateAnswerRequest.builder()
                .questionId(questionId)
                .title(title)
                .content(content)
                .file(file)
                .statusApproval(statusApproval)
                .roleConsultantId(roleConsultant.getId()) 
                .consultantId(user.getId()) 
                .build();

        // Gọi service để xử lý câu trả lời
        AnswerDTO answerDTO = answerService.createAnswer(answerRequest);

        // Trả về phản hồi API
        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder()
                .status("success")
                .message("Answer created successfully.")
                .data(answerDTO)
                .build());
    }
    
    @PostMapping("/review")
    public ResponseEntity<DataResponse<AnswerDTO>> reviewAnswer(
            @RequestBody CreateAnswerRequest reviewRequest, Principal principal) {

        // Lấy username của người dùng hiện tại
        String username = principal.getName();

        // Lấy thông tin người dùng từ cơ sở dữ liệu
        Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
        if (userOpt.isEmpty()) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("User not found.")
                    .build()
            );
        }

        UserInformationEntity user = userOpt.get();

        // Kiểm tra xem người dùng có phải là TRUONGBANTUVAN không
        if (!isConsultant(user)) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("You do not have permission to review this answer.")
                    .build()
            );
        }

        // Lấy thông tin câu trả lời
        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(reviewRequest.getQuestionId());
        if (answerOpt.isEmpty()) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("No answer found for this question.")
                    .build()
            );
        }

        AnswerEntity answer = answerOpt.get();

        // Kiểm tra nếu statusAnswer là true thì không cho phép trả lời lại
        if (answer.getStatusAnswer() != null && answer.getStatusAnswer()) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("This answer has already been approved and cannot be reviewed again.")
                    .build()
            );
        }

        // Lấy tư vấn viên đã trả lời câu hỏi này
        UserInformationEntity consultant = answer.getUser();

        // Kiểm tra xem trưởng ban có cùng department với tư vấn viên hay không
        if (!consultant.getAccount().getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("You do not have permission to review answers from another department.")
                    .build()
            );
        }

        // Gọi service để xử lý kiểm duyệt nếu người dùng có quyền và cùng department
        AnswerDTO reviewedAnswer = answerService.reviewAnswer(reviewRequest);

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder()
            .status("success")
            .message("Kiểm duyệt thành công")
            .data(reviewedAnswer)
            .build());
    }


    
    // Hàm kiểm tra vai trò của người dùng
    private boolean isConsultant(UserInformationEntity user) {
        return "TRUONGBANTUVAN".equals(user.getAccount().getRole().getName());
    }


}
