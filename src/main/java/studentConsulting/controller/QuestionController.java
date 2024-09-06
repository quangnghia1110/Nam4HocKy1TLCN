package studentConsulting.controller;

import java.security.Principal;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.model.payload.dto.MyQuestionDTO;
import studentConsulting.model.payload.dto.QuestionDTO;
import studentConsulting.model.payload.dto.RoleAskDTO;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.IQuestionService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("/api/v1/question")
public class QuestionController {
    
    @Autowired
    private IQuestionService questionService;
    
    @Autowired
    private IUserService userService;
    
    @PostMapping(value = "/create", consumes = {"multipart/form-data"})
    public ResponseEntity<DataResponse<QuestionDTO>> createQuestion(
            @RequestParam("departmentId") Integer departmentId,
            @RequestParam("fieldId") Integer fieldId,
            @RequestParam("roleAskId") Integer roleAskId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestParam("firstName") String firstName,
            @RequestParam("lastName") String lastName,
            @RequestParam("studentCode") String studentCode,
            @RequestParam("statusPublic") Boolean statusPublic,
            @RequestPart("file") MultipartFile file) {

        // Tạo đối tượng request mà không cần parentQuestionId
        CreateQuestionRequest questionRequest = CreateQuestionRequest.builder()
                .departmentId(departmentId)
                .fieldId(fieldId)
                .roleAskId(roleAskId)
                .title(title)
                .content(content)
                .firstName(firstName)
                .lastName(lastName)
                .studentCode(studentCode)
                .statusPublic(statusPublic)
                .file(file)
                .build();

        // Gọi service để tạo câu hỏi
        return ResponseEntity.ok(questionService.createQuestion(questionRequest));
    }


    
    @PostMapping(value = "/update", consumes = {"multipart/form-data"})
    public ResponseEntity<DataResponse<QuestionDTO>> updateQuestion(
            @RequestParam("questionId") Integer questionId,
            @RequestParam("departmentId") Integer departmentId,
            @RequestParam("fieldId") Integer fieldId,
            @RequestParam("roleAskId") Integer roleAskId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestParam("firstName") String firstName,
            @RequestParam("lastName") String lastName,
            @RequestParam("studentCode") String studentCode,
            @RequestParam("statusPublic") Boolean statusPublic,
            @RequestPart(value = "file", required = false) MultipartFile file) {

        UpdateQuestionRequest questionRequest = UpdateQuestionRequest.builder()
                .departmentId(departmentId)
                .fieldId(fieldId)
                .roleAskId(roleAskId)
                .title(title)
                .content(content)
                .firstName(firstName)
                .lastName(lastName)
                .studentCode(studentCode)
                .statusPublic(statusPublic)
                .file(file)
                .build();

        return ResponseEntity.ok(questionService.updateQuestion(questionId, questionRequest));
    }
    
    @DeleteMapping("/delete/{id}")
    public ResponseEntity<DataResponse<Void>> deleteQuestion(@PathVariable("id") Integer questionId) {
        return ResponseEntity.ok(questionService.deleteQuestion(questionId));
    }
    
    @GetMapping("/roleAsk")
    public ResponseEntity<DataResponse<List<RoleAskDTO>>> getAllRoleAsk() {
        List<RoleAskDTO> roleAsks = questionService.getAllRoleAsk();
        DataResponse<List<RoleAskDTO>> response = DataResponse.<List<RoleAskDTO>>builder()
                .status("success")
                .message("Fetched all role ask successfully.")
                .data(roleAsks)
                .build();

        return ResponseEntity.ok(response);
    }

    @PostMapping(value = "/create-follow-up", consumes = {"multipart/form-data"})
    public ResponseEntity<DataResponse<QuestionDTO>> askFollowUpQuestion(
            @RequestParam("parentQuestionId") Integer parentQuestionId,  // Nhận parentQuestionId
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestPart(value = "file", required = false) MultipartFile file) {

        // Gọi service để tạo câu hỏi follow-up
        return ResponseEntity.ok(questionService.askFollowUpQuestion(parentQuestionId, title, content, file));
    }

    @GetMapping("/user")
    public ResponseEntity<DataResponse<List<MyQuestionDTO>>> getQuestionsByUser(Principal principal) {
        // Lấy username từ Principal và sau đó lấy userId từ UserService
        String username = principal.getName();
        Integer userId = userService.getUserIdByUsername(username);

        // Gọi service để lấy danh sách câu hỏi của người dùng
        List<MyQuestionDTO> questions = questionService.getQuestionsByUserId(userId);

        // Kiểm tra nếu danh sách câu hỏi rỗng
        if (questions.isEmpty()) {
            DataResponse<List<MyQuestionDTO>> errorResponse = DataResponse.<List<MyQuestionDTO>>builder()
                    .status("error")
                    .message("No questions found for user ID: " + userId)
                    .build();
            return ResponseEntity.status(404).body(errorResponse);
        }

        // Trả về danh sách câu hỏi
        DataResponse<List<MyQuestionDTO>> response = DataResponse.<List<MyQuestionDTO>>builder()
                .status("success")
                .message("Fetched questions and answers for user successfully.")
                .data(questions)
                .build();

        return ResponseEntity.ok(response);
    }

    
    @GetMapping("/search")
    public ResponseEntity<DataResponse<List<MyQuestionDTO>>> searchQuestionsByTitle(
            @RequestParam("title") String title,
            Principal principal) {

        // Lấy username từ Principal và sau đó lấy userId từ UserService
        String username = principal.getName();
        Integer userId = userService.getUserIdByUsername(username);

        // Gọi service để tìm kiếm câu hỏi theo userId và tiêu đề
        List<MyQuestionDTO> questions = questionService.searchQuestionsByTitle(userId, title);

        if (questions.isEmpty()) {
            DataResponse<List<MyQuestionDTO>> errorResponse = DataResponse.<List<MyQuestionDTO>>builder()
                    .status("error")
                    .message("No questions found with title: " + title)
                    .build();
            return ResponseEntity.status(404).body(errorResponse);
        }

        DataResponse<List<MyQuestionDTO>> response = DataResponse.<List<MyQuestionDTO>>builder()
                .status("success")
                .message("Found questions successfully.")
                .data(questions)
                .build();

        return ResponseEntity.ok(response);
    }

    @GetMapping("/filter-by-department/{departmentId}")
    public ResponseEntity<DataResponse<List<MyQuestionDTO>>> filterQuestionsByDepartment(
            @PathVariable("departmentId") Integer departmentId,
            Principal principal) {

        // Lấy username từ Principal và sau đó lấy userId từ UserService
        String username = principal.getName();
        Integer userId = userService.getUserIdByUsername(username);

        // Gọi service để lọc câu hỏi theo userId và departmentId
        List<MyQuestionDTO> questions = questionService.filterQuestionsByDepartment(userId, departmentId);

        if (questions.isEmpty()) {
            DataResponse<List<MyQuestionDTO>> errorResponse = DataResponse.<List<MyQuestionDTO>>builder()
                    .status("error")
                    .message("No questions found for the given department.")
                    .build();
            return ResponseEntity.status(404).body(errorResponse);
        }

        DataResponse<List<MyQuestionDTO>> response = DataResponse.<List<MyQuestionDTO>>builder()
                .status("success")
                .message("Filtered questions by department successfully.")
                .data(questions)
                .build();

        return ResponseEntity.ok(response);
    }

    // Lọc câu hỏi theo trạng thái (status) (sử dụng PathVariable)
    @GetMapping("/filter-by-status")
    public ResponseEntity<DataResponse<List<MyQuestionDTO>>> filterQuestionsByStatus(
            @RequestParam("statusApproval") Boolean statusApproval,
            @RequestParam("statusPublic") Boolean statusPublic,
            @RequestParam("statusDelete") Boolean statusDelete,
            Principal principal) {

        // Lấy username từ Principal và sau đó lấy userId từ UserService
        String username = principal.getName();
        Integer userId = userService.getUserIdByUsername(username);

        // Gọi service để lọc câu hỏi theo userId và các trạng thái
        List<MyQuestionDTO> questions = questionService.filterQuestionsByCombinedStatus(
                userId, statusApproval, statusPublic, statusDelete);

        if (questions.isEmpty()) {
            DataResponse<List<MyQuestionDTO>> errorResponse = DataResponse.<List<MyQuestionDTO>>builder()
                    .status("error")
                    .message("No questions found for the given status combinations.")
                    .build();
            return ResponseEntity.status(404).body(errorResponse);
        }

        DataResponse<List<MyQuestionDTO>> response = DataResponse.<List<MyQuestionDTO>>builder()
                .status("success")
                .message("Filtered questions by status successfully.")
                .data(questions)
                .build();

        return ResponseEntity.ok(response);
    }
}

